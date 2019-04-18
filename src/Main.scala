import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Reg {
  var n = ""
  var d = ""
  def this(tn:String, td:String) { this()
    n = tn
    d = td
  }

}
object store {
  val keyWords = Array[String]("NEW", "REG", "SET", "ADD", "SUB")
  var tokens = new ArrayBuffer[Token]
  var rnames = new ArrayBuffer[Reg]
  var f = "NONE"

  var ram = Array.fill[Int](99)(0)
}

object Main extends App {
  println("LiTH")
  print("| Select Script: ")
  val f = Reader.read(scala.io.StdIn.readLine())

  Lexer.tokenize(f)
  Exe.exe()
}

object Reader {
  def read(f: String): ArrayBuffer[String] = {
    store.f = f
    println(s"| Reading Script : $f")

    // Get Items
    val x = Source.fromFile(f).getLines.toArray
    val ab = new ArrayBuffer[String]
    for { i <- 0 until x.length } {
      for { j <- 0 until x(i).split(" ").length } {
        ab += x(i).split(" ")(j)
      }
    }

    //CLEANUP
    for { i <- ab.length-1 to 0 } {
      if (ab(i) == "" || ab(i) == "\n") {}
      ab -= ab(i)
    }

    return ab
  }
}

object Lexer {
  def tokenize(f: ArrayBuffer[String]): Unit = {
    for { i <- 0 until f.length } {
      if (f(i).length >= 1) {
        for {k <- 0 until store.keyWords.length} {
          if (f(i) == store.keyWords(k)) {
            //println(s"Keyword Found: ${store.keyWords(k)}")
            store.tokens += new Token(store.keyWords(k), "NONE")
          }
        }
        if (f(i)(0).toString == "[") {
          //println(s"REGNAME FOUND: ${f(i)}")
          store.tokens += new Token("REGNAME", f(i).slice(1, f(i).length-1))
          //println(f(i).slice(1, f(i).length-1))
        }
        else if (f(i)(0).toString == "$") {
          //println(s"NUMBER FOUND: ${f(i).slice(1, f(i).length)}")
          store.tokens += new Token("DATA", f(i).slice(1, f(i).length))
        }
      }
    }
  }
}

object Exe {
  def exe(): Unit = {
    import store.tokens
    import store.ram

    var error = false
    var errorCode = ""

    println("|")
    println(s"| LiTH : ${store.f}")
    println("|")
    println("| WARNINGS:")

    // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR //
    var i = 0
    while ( i < tokens.length ) {
      if (tokens(i).t == "NEW") {
        i += 1
        if (tokens(i).t == "REG") {
          i += 1
          var rexist = false
          for { j <- 0 until store.rnames.length } {
            if (store.rnames(j).n == tokens(i).n) {
              rexist = true
            }
          }
          if (rexist) {
            println(s"| ${tokens(i).n} Already Exists : Skipped Recreation")
          } else {
            //println(s"| ${tokens(i).n} : Created")
            store.rnames += new Reg(tokens(i).n, "NONE")
          }
        }
      } else if (tokens(i).t == "SET") {
        i += 1
        if (tokens(i).t == "REGNAME") {
          var rpos = 0
          var rexist = false
          for { j <- 0 until store.rnames.length } {
            if (tokens(i).n == store.rnames(j).n) {
              rpos = j
              rexist = true
            }
          }
          if (!rexist) {
            error = true
            errorCode = s"Register ${tokens(i).n} Does Not Exist : Please Create The Register Before Setting A Value"
            i = tokens.length+1 //Breaks Code
          } else {
            i += 1
            if (tokens(i).t == "DATA") {
              store.rnames(rpos).d = tokens(i).n
            }
          }
        }
      }

      i += 1
    }
    // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR //
    //Display RAM
    if (!error) {
      println("|")
      println("| OUTPUT: ")
      println(s"| RAM SIZE : ${ram.length + 1}b")
      var s = ""
      for {i <- 0 until ram.length} {
        s += "[" ++ ram(i).toString ++ "]"
        if (s.length == 25 * 3) {
          println("| " ++ s)
          s = ""
        }
      }
      s += "[" ++ ram(ram.length - 1).toString ++ "]"
      println("| " ++ s)
      //Display REG
      println("|")
      for {i <- 0 until store.rnames.length} {
        println("| [" ++ store.rnames(i).n ++ "] : " ++ store.rnames(i).d)
      }
    } else {
      println("|")
      println(s"| Code Stopped In Error: ")
      println(s"| ${errorCode}")
    }
  }
}

class Token {
  var t = "NONE"
  var n = "NONE"
  def this(ty: String, na: String) {this()
    t = ty
    n = na
  }
}
