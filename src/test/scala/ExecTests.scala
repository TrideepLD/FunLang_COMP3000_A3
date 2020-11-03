/**
 * FunLang language execution tests.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import FunLangTree.ExpTree

/**
 * Tests that check that the translation works correctly.
 */
class ExecTests extends SemanticTests {

    import org.bitbucket.inkytonik.kiama.util.StringEmitter

    /**
     * Parse some test input, perform semantic analysis checks, expect no
     * semantic errors. Then translate into SEC machine code and run the code.
     * The value `expected` should be the output that we expect from this
     * run.
     */
    def execTest (str : String, expected : String) {
        val tree = parseProgram (str)
        val analysis = new SemanticAnalysis (new ExpTree (tree))
        import analysis._
        val messages = analysis.errors (tree)
        // println (messages)
        assert (messages.length === 0)

        val instrs = Translator.translate (tree)
        // println (instrs)

        val emitter = new StringEmitter ()
        val machine = new SECMachine (emitter)

        machine.run (instrs) match {
            case _ : machine.State =>
                // Terminated correctly in some state
                assertResult (expected + "\n", "wrong execution output") (emitter.result ())
            case machine.FatalError (message) =>
                fail (message)
        }
    }

    test ("an integer expression evaluates to the correct result") {
        execTest ("""
            |1
            """.stripMargin,
            "1")
    }

    test ("an addition expression evaluates to the correct result") {
        execTest ("""
            |3 + 4
            """.stripMargin,
            "7")
    }

    // FIXME: more tests here...

    // I'm gonna have strong incredible tests
    // And they're gonna look at my code legally(on Github).
    // We're gonna build a private repo
    // It's going to be built.
    // Believe it or not it's not even a difficult thing to do.
    // And we'll make github host it privately for us.

    // Testing BoolExp
    test ("a true expression evaluates to the correct result") {
        execTest ("""
            |true
            """.stripMargin,
            "true")
    }

    test ("a false expression evaluates to the correct result") {
        execTest ("""
            |false
            """.stripMargin,
            "false")
    }

    // Testing Plus,Minus,Star,Slash,Equal,Less EXP
    test ("a subtraction expression evaluates to the correct result") {
        execTest ("""
            |3 - 4
            """.stripMargin,
            "-1")
    }

    test ("a multiplication expression evaluates to the correct result") {
        execTest ("""
            |5 * 3
            """.stripMargin,
            "15")
    }

    test ("a division expression evaluates to the correct result") {
        execTest ("""
            |12 / 5
            """.stripMargin,
            "2")
    }

    test ("an equality expression evaluates to the correct result (true)") {
        execTest ("""
            |12 == 5
            """.stripMargin,
            "false")
    }

    test ("an equality expression evaluates to the correct result (false)") {
        execTest ("""
            |9 == 9
            """.stripMargin,
            "true")
    }

    test ("a less-than expression evaluates to the correct result (true)") {
        execTest ("""
            |8 < 9
            """.stripMargin,
            "true")
    }

    test ("a less-than expression evaluates to the correct result (false)") {
        execTest ("""
            |9 < 8
            """.stripMargin,
            "false")
    }

    // Arithmetic operation tests

    test ("a multi-operator expression evaluates to the correct result") {
        execTest ("""
            |3 * (12 / 4)
            """.stripMargin,
            "9")
    }

    test ("simple expression evaluates to the correct result") {
        execTest ("""
            |57 + 3
            """.stripMargin,
            "60")

        execTest ("""
            |57 - 7
            """.stripMargin,
            "50")

        execTest ("""
            |27 / 9
            """.stripMargin,
            "3")

        execTest ("""
            |4 * 7
            """.stripMargin,
            "28")

    }

    // Logial Operations
    test ("10 is not < 2") {
        execTest ("""
            |10 < 2
            """.stripMargin,
            "false")
    }

    test ("10 is < 20") {
        execTest ("""
            |10 < 20
            """.stripMargin,
            "true")
    }

    test ("10 is not == 2") {
        execTest ("""
            |10 == 2
            """.stripMargin,
            "false")
    }

    test ("10 is == 10") {
        execTest ("""
            |10 == 10
            """.stripMargin,
            "true")
    }

    // We're gonna build an Associativity and Precedence test
    // A test so very grea, it will test Associativity and Precedence
    // It will be the greatest Associativity and Precedence test

    test ("evaluation of operators is in the correct order") {
		execTest ("""
			|1 / 2 == 2 / 1
			""".stripMargin,
			"false")
		execTest ("""
			|1 / 2 < 2 / 1
			""".stripMargin,
			"true")
		execTest ("""
			|1 + 2 * 3
			""".stripMargin,
			"7")
		execTest ("""
			|2 * 3 - 1
			""".stripMargin,
            "5")
        // Precedence of * and / are same and Associativity is left so 20*2, then 40/2 then 20/2 then 10*5
        execTest ("""
            |20*2/2/2*5+10-20
            """.stripMargin,
            "40")
        execTest ("""
            |20*2/2/2*5+10-20 < 50*50+20
            """.stripMargin,
            "true")
        execTest ("""
            |20*2/2/2*5+10-20 < 50*50-5000/4500-2500
            """.stripMargin,
            "false")
        execTest ("""
            |20*2/2/2*5+10-20 == 20+20
            """.stripMargin,
            "true")
        execTest ("""
            |20*2/2/2*5+10-20 == 40+20
            """.stripMargin,
            "false")
        execTest ("""
            |20*2/2/2*5+10-20 < 10000
            """.stripMargin,
            "true")
        execTest ("""
            |20*2/2/2*5+10-20 < 0
            """.stripMargin,
            "false")
    }

    // If-Statement tests
    //  Standard template for if statements is if-then-else, so if-(if-then-else)-else which can become if-(if-(if-then-else)-else)-else

    test ("an if expression evaluates to the correct result") {
        execTest ("""
            |if(true) then 1 else 0
            """.stripMargin,
            "1")

        execTest ("""
            |if(false) then 1 else 0
            """.stripMargin,
            "0")

    }

    test ("a true less-than condition evaluates to the correct result") {
        execTest ("""
            |if (1 < 2) then 15 else 0
            """.stripMargin,
            "15")
    }

    test ("a false less-than condition evaluates to the correct result") {
        execTest ("""
            |if (4 < 2) then 15 else 0
            """.stripMargin,
            "0")
    }

    test ("a false equal condition evaluates to the correct result") {
        execTest ("""
            |if (1 == 2) then 0 else 12
            """.stripMargin,
            "12")
    }

    test ("a true equal condition evaluates to the correct result") {
        execTest ("""
            |if (1 == 1) then 0 else 12
            """.stripMargin,
            "0")
    }

    test ("a true literal condition evaluates to the correct result") {
        execTest ("""
            |if (true) then 8 else 12
            """.stripMargin,
            "8")
    }

    test ("nested if statments evaluate to the correct result") {
        execTest ("""
            |if (1 < 0) then 
            |	if (2 < 3) then 101 else 102
            |else
            |	if (4 < 7) then 201 else 202
            """.stripMargin,
            "201")
    }

    test ("super-duper nested if statement") {
        execTest ("""
            |if (1 < 0) then 
            |	if (2 < 3) then 101 else 102
            |else
            |   if (9 < 10) then 
            |       if (2 < 3) then 
            |           if ( 10 < 9 ) then 
            |               if (20 < 39) then 201 else 202
            |           else 501
            |       else 401
            |   else 301
            |       
            """.stripMargin,
            "501")
    }


    // I know blockExp tests
    // I have the best blockExp tests
    // People always tell me that I have the best blockexp tests
    

    // BlockExp test below
    test ("Testing block expressions to show they produce the right result") {
        execTest ("""
            |{
            |   val a = 10
            |   val b = 15
            |   if (a < b) then (a + b) else (a-b)
            |}
            """.stripMargin,
            "25")

        execTest ("""
            |{
            |   val x = 5
            |   def square (a:Int) = a*a
            |   square (x)
            |}
            """.stripMargin,
            "25")
    }

    // I have apple, I have pen, applepen
    test ("I have an If, I have a function, IfFunction!") {
        execTest ("""
            |{
            |   val x = 5
            |   def disccriminate_against_zeros (a:Int) = if (a==0) then 1 else a
            |   disccriminate_against_zeros (x)
            |}
            """.stripMargin,
            "5")
    }

    test ("I have a function, I have a function, FunctionFunction") {
        execTest ("""
            |{
            |   val x = 5
            |   def square (a:Int) = a*a
            |   def inc (a:Int) = a+1
            |   square (x) + inc (x)
            |}
            """.stripMargin,
            "31")

        execTest ("""
            |{
            |   val x = 2
            |   def square (a:Int) = a*a
            |   def squrePlus (a:Int) = square (a) + square (a)
            |   squrePlus (x)
            |}
            """.stripMargin,
            "8")
    }

    test ("I have a ifFunction, I have a FunctionFunction, IfFunction-FunctionFunction") {
        execTest ("""
            |{
            |   val x = 5
            |   def disccriminate_against_zeros (a:Int) = if (a==0) then 1 else a
            |   def square (a:Int) = a*a
            |   def inc (a:Int) = a+1
            |   square (x) + inc (x) + disccriminate_against_zeros (x)
            |}
            """.stripMargin,
            "36")

        execTest ("""
            |{
            |   val x = 2
            |   def square (a:Int) = a*a
            |   def anIfFunction (a:Int) = if (a==0) then 1 else square (a) + square (a)
            |   anIfFunction(x)
            |}
            """.stripMargin,
            "8")
    }
}

