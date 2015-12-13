/**
 * Basic example using an Object for printing stuff.
 */

object GettingStarted {
    def abs(x: Int): Int =
        if (x < 0)
            -x
        else
            x

    def fact(n: Int): Int = {
        @annotation.tailrec
        def loop(n: Int, acc: Int): Int = if (n < 1) acc else loop(n-1, n*acc)
        loop(n, 1)
    }

    def fib(n: Int): Int = {
        def loop(n: Int): Int =
            if (n == 1) 0
            else if (n == 2) 1
            else loop(n-2) + loop(n-1)
        loop(n)
    }

    /** Exercise 2.1 */
    def fibtail(n: Int): Int = {
        @annotation.tailrec
        def loop(n: Int, p: Int, c: Int): Int =
            if (n == 1) p
            else loop(n-1, c, p + c)
        loop(n, 0, 1)
    }

    /** Exercise 2.2 */
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int): Boolean =
            if (n+1 >= as.length) true
            else if (!ordered(as(n), as(n+1))) false
            else loop(n+1)
        loop(0)
    }

    private def formatResult(n: String, x: Int, f: Int => Int): String =
        "%s of %d is %d".format(n, x, f(x))

    def main(args: Array[String]): Unit = {
        println(formatResult("Absolute value", -42, abs))
        println(formatResult("Factorial", 5, fact))
        println(formatResult("Fib", 8, fib))
        println(formatResult("FibTail", 8, fibtail))

        println(isSorted(Array(2, 4, 6), (a: Int, b: Int) => a < b))
        println(isSorted(Array("A", "BB", "C"), (a: String, b: String) => a.length < b.length))
    }
}

/* Because we define other than the object
 * the main it's not called automatically. */
GettingStarted.main(Array())

/** Exercise 2.3 */
def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

/** Exercise 2.4 */
def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

/** Exercise 2.5 */
def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

def main(args: Array[String]): Unit = {
    val sum = (a: Int, b: Int) => a + b

    val cur = curry(sum)
    val unc = uncurry(cur)

    val inc = cur(1)

    var two_1 = inc(1)
    val two_2 = unc(1, 1)

    println(two_1, two_2)

    val multiply = (a: Int, b: Int) => a * b
    val mult_by_two = curry(multiply)(2)
    val sum_one_and_mult_by_two = compose(mult_by_two, inc)

    println(sum_one_and_mult_by_two(2))
}

main(Array())

