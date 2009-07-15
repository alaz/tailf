package com.osinka.tailf

object Main {
    def main(args: Array[String]): Unit = {
        import java.io.{File, BufferedReader, InputStreamReader}

        val f = new File(args(0))
        val r = new BufferedReader(new InputStreamReader(Tail.follow(f)))

        def read: Unit = {
            val l = r.readLine;
            if (l != null) {
                println(l);
                read
            }
        }

        read
    }
}
