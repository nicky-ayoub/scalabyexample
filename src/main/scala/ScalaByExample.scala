package nickyayoub.com

object ScalaByExample {
   
   def abs(x: Double) = if (x >= 0) x else -x

   def square(x: Double) = x * x

   def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

   def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)

   def factorialt(n: Int): Int =  {
       def fact(n: Int, acc: Int): Int = {
       	   if( n == 0)  acc else fact(n-1, acc*n)
       }
       return fact(n, 1);
   }

   def sqrt(x: Double) = {
      def sqrtIter(guess: Double): Double =
         if (isGoodEnough(guess)) guess
         else sqrtIter(improve(guess))

      def improve(guess: Double) =
         (guess + x / guess) / 2

      def isGoodEnough(guess: Double) =
         (abs(square(guess) - x)/x) < 0.00001

       sqrtIter(1.0)
   }

   def main(args: Array[String]) {
      // do something
      println (sqrt(2));
      println (sqrt(4));
      println (sqrt(20.0));
      println (gcd(14,21));
      println (factorial(10));
      println (factorialt(10));
   }
}

class ScalaByExample  {
  // do something

}