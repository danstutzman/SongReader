import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream
import java.io.PrintWriter
import javax.mail.internet.MimeBodyPart
import javax.mail.internet.MimeMessage
import javax.mail.Multipart
import javax.mail.Part
import javax.mail.Session

def saveAttachments(
    part:Part, filenamePrefix:String, level:Int, num:Int, outputDir:File) {
  if (part.isMimeType("text/plain")) {
    // skip it
  } else if (part.isMimeType("multipart/*")) {
    val multipart = part.getContent().asInstanceOf[Multipart]
    (0 until multipart.getCount()).foreach { i =>
      saveAttachments(multipart.getBodyPart(i), filenamePrefix,
        level + 1, num + i, outputDir)
    }
  } else if (part.isMimeType("message/rfc822")) {
    saveAttachments(
      part.getContent().asInstanceOf[Part], filenamePrefix,
        level + 1, num, outputDir)
  } else if (part.isMimeType("image/jpeg")) {
    val jpegPath = new File(outputDir, "%s.%d.jpeg".format(filenamePrefix, num))
    part.asInstanceOf[MimeBodyPart].saveFile(jpegPath)
    println("-> %s".format(jpegPath))

    val jsonPath = new File(outputDir, "%s.%d.json".format(filenamePrefix, num))
    val jsonWriter = new PrintWriter(jsonPath)
    jsonWriter.print("[]\n")
    jsonWriter.close()
  } else {
    printf("Ignoring content type %s\n",
      part.getContentType().replaceAll("[\r\n\t ]+", " "))
  }
}

val existingImageNames = new File("input").list()

val session = Session.getInstance(System.getProperties(), null)
new File("getmail/new").listFiles().foreach { messageFile =>
  if (messageFile.getName() != ".gitkeep") {
    println("Reading %s...".format(messageFile))
    val message = new MimeMessage(session,
      new BufferedInputStream(new FileInputStream(messageFile)))
    if (!existingImageNames.exists { _.startsWith(messageFile.getName()) }) {
      saveAttachments(message, messageFile.getName(), 0, 0, new File("input"))
    }
  }
}
