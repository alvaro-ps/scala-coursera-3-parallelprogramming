package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {
  
  @Test def `test clamp`: Unit = {
    assertEquals(clamp(5, 0, 10), 5)
    assertEquals(clamp(5, 0, 2), 2)
    assertEquals(clamp(-2, 0, 10), 0)
  }

  @Test def `test Filter`: Unit = {
    val i = new Img(width=3, height=3, data=Array(
        4, 4, 4,
        4, 4, 4, 
        1, 1, 1
      )
    )

    assertEquals(4, boxBlurKernel(i, 1, 0, 1))
    assertEquals(3, boxBlurKernel(i, 0, 1, 1))
    assertEquals(3, boxBlurKernel(i, 1, 1, 1))
  }

  @Test def `test vertical Blur`: Unit = {
    val src = new Img(width=3, height=3, data=Array(
        2, 0, 0,
        2, 0, 0, 
        2, 0, 0
      )
    )
    val dst = new Img(width=3, height=3, data=Array(
        0, 0, 0,
        0, 0, 0, 
        0, 0, 0
      )
    )
    val expected = new Img(width=3, height=3, data=Array(
        1, 0, 0,
        1, 0, 0, 
        1, 0, 0
      )
    )
    VerticalBoxBlur.blur(src, dst, from=0, end=1, radius=1)
    for (x <- 0 until dst.width; y <- 0 until dst.height)
      assertEquals(expected(x, y), dst(x, y))
  }

  @Test def `test horizontal Blur`: Unit = {
    val src = new Img(width=3, height=3, data=Array(
        2, 0, 0,
        2, 0, 0, 
        2, 0, 0
      )
    )
    val dst = new Img(width=3, height=3, data=Array(
        0, 0, 0,
        0, 0, 0, 
        0, 0, 0
      )
    )
    val expected = new Img(width=3, height=3, data=Array(
        1, 0, 0,
        0, 0, 0, 
        0, 0, 0
      )
    )
    HorizontalBoxBlur.blur(src, dst, from=0, end=1, radius=1)
    for (x <- 0 until dst.width; y <- 0 until dst.height)
      assertEquals(expected(x, y), dst(x, y))
  }

  @Test def `test vertical parBlur`: Unit = {
    val src = new Img(width=3, height=3, data=Array(
        2, 0, 0,
        2, 0, 0, 
        2, 0, 0
      )
    )
    val dst = new Img(width=3, height=3, data=Array(
        0, 0, 0,
        0, 0, 0, 
        0, 0, 0
      )
    )
    val expected = new Img(width=3, height=3, data=Array(
        1, 0, 0,
        1, 0, 0, 
        1, 0, 0
      )
    )
    VerticalBoxBlur.parBlur(src, dst, numTasks=1, radius=1)
    for (x <- 0 until dst.width; y <- 0 until dst.height)
      assertEquals(expected(x, y), dst(x, y))
  }

  @Test def `test horizontal parBlur`: Unit = {
    val src = new Img(width=3, height=3, data=Array(
        2, 0, 0,
        2, 0, 0, 
        2, 0, 0
      )
    )
    val dst = new Img(width=3, height=3, data=Array(
        0, 0, 0,
        0, 0, 0, 
        0, 0, 0
      )
    )
    val expected = new Img(width=3, height=3, data=Array(
        1, 0, 0,
        1, 0, 0, 
        1, 0, 0
      )
    )
    HorizontalBoxBlur.parBlur(src, dst, numTasks=1, radius=1)
    for (x <- 0 until dst.width; y <- 0 until dst.height)
      assertEquals(expected(x, y), dst(x, y))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
