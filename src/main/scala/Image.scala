import java.io.File
import javax.imageio.ImageIO
import mohs.Mohs
import mohs.prettyPrint
import mohs.given
import javax.imageio.ImageWriter
import java.awt.image.BufferedImage

@main def blur() =
    val file: File = new File("cat.png")
    val image = ImageIO.read(file)

    val width = image.getWidth
    val height = image.getHeight
    val pixels: Array[Int] = image.getRGB(0, 0, width, height, null, 0, width)

    val array = Mohs.List[Int](pixels.flatMap(x => Array((x>>16)&0xff, (x>>8)&0xff, (x>>0)&0xff)))
        .reshape(height, width, 3)

    val blurred = mohs.UFunc.toInt(array.windows(3,3)
        .axis(2).flatMap(_.cells(2).reduce(_+_)/9))

    val outputFile: File = new File("blur.png")
    val outputImage: BufferedImage = new BufferedImage(width-2, height-2, BufferedImage.TYPE_INT_ARGB)

    val outputPixels = blurred.axis(2).map(x => {
        x.zip(Mohs.List(16, 8, 0)).map(_<<_).reduce(_|_)+(255<<24)
    })

    outputImage.setRGB(0, 0, width-2, height-2, outputPixels.values, 0, width-2);

    ImageIO.write(outputImage,"png",outputFile)