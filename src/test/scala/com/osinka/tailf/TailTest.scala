package com.osinka.tailf

import java.io.File
import org.specs._
import org.specs.runner._

class TailSpecTest extends JUnit4(TailSpec) with Console
//class TailSpecSuite extends ScalaTestSuite(TailSpec)
object TailSpecRunner extends ConsoleRunner(TailSpec)

object TailSpec extends Specification {
    val existingFile = new File(getClass.getClassLoader.getResource("existing.txt").toURI)
    val absentFile = new File("absent.txt")
    "File existance check" should {
        doBefore { absentFile.delete }
        doAfter  { absentFile.delete }

        "succeed on existing file" in {
            existingFile aka "double check that the existing file exists indeed" must exist
            Tail.testExists(existingFile, 0, () => {}) must beTrue
            Tail.testExists(existingFile, 1, () => {}) must beTrue
        }
        "fail on absent file" in {
            absentFile aka "double check that the absent file does not exist indeed" mustNot exist
            Tail.testExists(absentFile, 0, () => {}) must beFalse
            Tail.testExists(absentFile, 1, () => {}) must beFalse
        }
        "succeed after create" in {
            def testCreate(f: File, n: Int) = try {
                var count = 0
                val result = Tail.testExists(f, n, () => { count += 1; if (count >= n) f.createNewFile } )
                (result, count)
            } finally { f.delete }

            existingFile aka "double check that the existing file exists indeed" must exist
            absentFile aka "double check that the absent file does not exist indeed" mustNot exist
            testCreate(absentFile, 0) mustEqual (false, 0)
            testCreate(absentFile, 1) mustEqual (true, 1)
            testCreate(existingFile, 10) mustEqual (true, 0)
        }
    }

    val tf = new File("test.txt")
    val nf = new File("rotated.txt")
    "FollowingInputStream" should {
        doBefore { tf.delete }
        doAfter  { tf.delete }

        "read lines" in {
            import java.io.{FileWriter,InputStreamReader}
            val out = new FileWriter(tf)

            val ms = 5
            val t = new ReadThread(new FollowingInputStream(tf, () => sleepFunc(ms)))

            t.start; LineCollector.start

            def testLine(s: String) = { out.write(s); out.flush; sleepFunc(ms+10); LineCollector.get }
            testLine("test1\n") must be_==("test1\n")
            testLine("test2\n") must be_==("test1\ntest2\n")

            t.interrupt; LineCollector.stop; out.close
        }
    }
    "Follow.tail" should {
        doBefore { tf.delete; nf.delete }
        doAfter  { tf.delete; nf.delete }

        "survive rotate" in {
            import java.io.{FileWriter,InputStreamReader}
            var out = new FileWriter(tf)

            val ms = 25
            val t = new ReadThread(Tail.follow(tf, 2, () => sleepFunc(ms), () => sleepFunc(ms)))

            t.start; LineCollector.start

            def testLine(s: String) = { out.write(s); out.flush; sleepFunc(ms+5); LineCollector.get }
            testLine("test1111\n") must be_==("test1111\n")

            out.close; tf.renameTo(new File("rotated.txt")) must beTrue
            out = new FileWriter(tf)

            testLine("test2\n") must be_==("test1111\ntest2\n")

            t.interrupt; LineCollector.stop; out.close
        }
    }

    def sleepFunc(t: Long) = Thread.sleep(t)
}

import java.io.InputStream
import scala.actors._
import Actor._

case class Put(line: String)
case object Get

object LineCollector extends Actor {
    def get = (this !? Get).asInstanceOf[String]
    def stop = this ! Exit

    def act = run("")

    def run(v: String): Nothing = react {
        case Exit => exit
        case Put(line) => run(v+line+"\n")
        case Get => reply(v); run(v)
    }
}

class ReadThread(stream: InputStream) /* extends Thread */ {
    import java.io.{BufferedReader, InputStreamReader}
    private val in = new BufferedReader(new InputStreamReader(stream))
    @volatile private var interrupted = false

    def start = Scheduler.execute(read)

    def interrupt { interrupted = true }

    protected def isInterrupted = interrupted

    private def read: Unit = if (isInterrupted) in.close else try {
        val l = in.readLine;
        if (l != null) {
            LineCollector ! Put(l)
            read
        }
    } catch {
        case e: InterruptedException => in.close
    }
}
