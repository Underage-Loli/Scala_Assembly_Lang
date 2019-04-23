import java.util.Calendar

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
  var tstart = Calendar.getInstance().getTime()
  val keyWords = Array[String]("NEW", "REG", "SET", "ADD", "SUB", "RAM", "SAY", "DISPLAY", "WARN", "INPUT")
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
  def read(file: String): ArrayBuffer[String] = {
    store.tstart = Calendar.getInstance().getTime()
    store.f = file
    println(s"| Reading Script : $file")

    val f = "script/" ++ file

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
        else if(f(i)(0).toString  == "&") {
          store.tokens += new Token("REF", f(i).slice(1, f(i).length))
        }
      }
    }

    println("PRE-EXE:")
    for { i <- 0 until store.tokens.length } {
      if (store.tokens(i).t == "WARN") {
        println(s"| !:${store.tokens(i+1).n}")
      }
      else if (store.tokens(i).t == "INPUT") {
        print("| $:")
        store.tokens(i) = new Token("DATA", scala.io.StdIn.readLine())
      }
    }
    println("| DONE")
  }
}

object Exe {
  def exe(): Unit = {
    import store.tokens
    import store.ram

    var error = false
    var errorCode = ""

    var display = false

    println("|")
    println(s"| LiTH : ${store.f}")
    println("|")
    println("| OUTPUT:")

//    DEBUG - PRINT TOKENS
//
//    for { i <- 0 until tokens.length } {
//      println(tokens(i).t ++ " : " ++ tokens(i).n)
//    }

    // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR //
    var i = 0
    while ( i < tokens.length ) {
      if (tokens(i).t == "NEW") { //NEW-EXE
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
            store.rnames += new Reg(tokens(i).n, "0")
          }
        }
      } else if (tokens(i).t == "SAY") { //SAY-EXE
        i += 1
        if (tokens(i).t == "DATA") {
          println("| ~" ++ tokens(i).n)
        }
        else if(tokens(i).t == "REGNAME") {
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
            println("| ~" ++ store.rnames(rpos).d)
          }
        }
      } else if (tokens(i).t == "SET") { //SET-EXE
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
            else if(tokens(i).t == "REGNAME") {
              var rpos2 = 0
              var rexist2 = false
              for {j <- 0 until store.rnames.length} {
                if (tokens(i).n == store.rnames(j).n) {
                  rpos2 = j
                  rexist2 = true
                }
              }
              if (!rexist2) {
                error = true
                errorCode = s"Register ${tokens(i).n} Does Not Exist : Please Create The Register Before Setting A Value"
                i = tokens.length + 1 //Breaks Code
              } else {
                store.rnames(rpos).d = store.rnames(rpos2).d
              }
            }
          }
        }
      } else if (tokens(i).t == "REGNAME") { //REGNAME-EXE
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
          if (tokens(i).t == "ADD") {
            i += 1
            if (tokens(i).t == "DATA") {
              store.rnames(rpos).d = (store.rnames(rpos).d.toInt + tokens(i).n.toInt).toString
            }
            else if(tokens(i).t == "REGNAME") {
              var rpos2 = 0
              var rexist2 = false
              for { j <- 0 until store.rnames.length } {
                if (tokens(i).n == store.rnames(j).n) {
                  rpos2 = j
                  rexist2 = true
                }
              }
              if (!rexist2) {
                error = true
                errorCode = s"Register ${tokens(i).n} Does Not Exist : Please Create The Register Before Setting A Value"
                i = tokens.length+1 //Breaks Code
              } else {
                store.rnames(rpos).d = (store.rnames(rpos).d.toInt + store.rnames(rpos2).d.toInt).toString
              }
            }
          }
        }
      } else if (tokens(i).t == "DISPLAY") {
        display = true
      }
      i += 1
    }
    // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR // EXECUTOR //
    //Display RAM
    if (!error && display) {
      println("|")
      println("| DISPLAY: ")
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
      println("| REGISTERS:")
      for {i <- 0 until store.rnames.length} {
        println("| [" ++ store.rnames(i).n ++ "] : " ++ store.rnames(i).d)
      }
    } else {
      if (error) {
        println("|")
        println(s"| Code Stopped In Error: ")
        println(s"| $errorCode")
      }
    }

    //EXIT LiTH
    if (!error) {
      val tstop = Calendar.getInstance().getTime()

      println("|")
      println("| Code Stopped After Run Success")
      println("| START: " ++ store.tstart.toString)
      println("|  STOP: " ++ tstop.toString)
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

