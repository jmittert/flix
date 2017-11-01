def times(x: Int, y: Int):Int = x * y

def fact(x: Int):Int = if ( x == 0) 1 else x * fact(x-1)

def main(): Int = 
   let x = fact(5);
   let y = 456;
   let z = times(x,y);
   z - 201

