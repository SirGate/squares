/**
  *
  * @author Sergei Tulupov
  */

import java.io.IOException

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object squares {
var current_pos=new ArrayBuffer[Int]()                 //uses for storage of current calculated figure
val square = Array.ofDim[Int](12,4)        // uses for storage of current set of input data
val pos=new ArrayBuffer[Int]()                        //uses for storage of order of positions of squares for printing
pos+=(4,5,11,0,1,6,10,3,2,7,9,8)
var count=0
  def solution(input:Array[Int]) {           // function calculate how many sets of data in input file
  val quant=input.length/48                       // reads by one set in square and call function calc_comb
    for(k<-0 to quant-1){                         //for calculation of ever set
      for(j<-0 to 11;i<-1 to 4)
      square(j)(i-1)= input(i+k*4-1+j*4*quant)
      calc_comb
      }
    println("Solutions found: "+count)
    }

def calc_comb: Unit ={    // function calculate all combinations of four squares for centre of figure
  val a=new Array[Int](4)   // and transmits their numbers for checking by function calc_sum
  for(i<-0 to 8;j<-i+1 to 9; k<-j+1 to 10;m<-k+1 to 11)  //calculate all existed combinations from 12 squares by 4
   {a(0)=i;a(1)=j;a(2)=k;a(3)=m                       //them are  - 12!/8!*4!=495
  calc_sum(a)}
}

def calc_sum(a:Array[Int]): Unit ={  //for current combination from 4 squares sort out  all permutations-
val b=new Array[Int](a.length-1)     // them are 2*3*4=24 and check out which of them correct 
var sum:Int=0                        //for  correct calls function calc_beams for  check out of beams of figure
var check = true
  for(i<- a.indices){  // pass through all 4 squares in the diffirent combinations
    current_pos+=a(i) // add in current template number of square
    if(a.length>1){    //if quantity of remaining squares is more then 1
     var index=0
      for(j<-a.indices if j!=i){b(index)=a(j);index+=1}
           calc_sum(b)  // recurrently call themselves with data reduced on 1 square
    } else{    // if remains the final square,then will check out current permutation from 4 squares on the correctness
      sum=square(current_pos(0))(3)+square(current_pos(1))(2)+square(current_pos(2))(0)+square(current_pos(3))(1)
      if(square(current_pos(0))(1)+square(current_pos(1))(0)>10){check=false}
      if(square(current_pos(1))(3)+square(current_pos(2))(1)>10){check=false}
      if(square(current_pos(2))(2)+square(current_pos(3))(3)>10){check=false}
      if(square(current_pos(0))(2)+square(current_pos(3))(0)>10){check=false}
       if((sum==10)&(check)){//if the centre from 4 squares is fit
         val beams=new Array[Int](8) //transmit remainig 8 squares
         var index=0
         for(k<-0 to 11 if !current_pos.contains(k)){beams(index)=k;index+=1}
         calc_beams(beams)// and call function calc_beams for sort out beams of figure
		 }
      sum=0
      check=true
    }
     current_pos-=a(i)// delete back current  square from template by return from recursion
  }
}

def calc_beams(a:Array[Int]): Unit ={    // function sort out by 2 squares from 8, then 2 из 6 e.t.c.
        // add them to the template and check out correctness
var i1=0
var j1=0
  for(i<-0 to a.length-2){         //sort out all variations  of permutations - 2 from  n
  for(j<-i+1 to a.length-1){      // 2 from 8 = 8!/6!*2!=28, 2 from 6=15 variations e.t.c.
    if(a.length==8){              // if first beam
	i1=i;j1=j;
      for(m<-0 to 1){       //take into consideration ,that two squares should be changed places themselves 
      if((square(a(i1))(3)+square(a(j1))(2)+square(current_pos(0))(1)+square(current_pos(1))(0)==10)&
      (square(current_pos(0))(0)+square(a(i1))(2)<=10)&   //check out correctness
        (square(current_pos(1))(1)+square(a(j1))(3)<=10)&
        (square(a(i1))(1)+square(a(j1))(0)<=10))
      {
      var index=0  //if are fit
      val b=new Array[Int](a.length-2)
        for(k<-a.indices if((k!=i1)&(k!=j1))){b(index)=a(k);index+=1}
        current_pos+=a(i1)   //add them to the the current template
        current_pos+=a(j1)
        calc_beams(b)      // recurrently call ourselves with remaining number of the squares
		current_pos-=a(j1)      // delete from template by exit from recursion
        current_pos-=a(i1)
      }
      i1=i1^j1;j1=i1^j1;i1=i1^j1     // change two squares by places on the beam
    }
      i1=0
      j1=0;
    }
    if(a.length==6){ //if the second beam ,then all the same like for first beam
      i1 = i; j1 = j
        for (m <- 0 to 1) {
          if ((square(a(i1))(2) + square(a(j1))(0) + square(current_pos(1))(3) + square(current_pos(2))(1) == 10) &
            (square(current_pos(1))(1) + square(a(i1))(0) + square(current_pos(5))(3) <= 10) &
            (square(current_pos(2))(3) + square(a(j1))(2) <= 10) &
            (square(a(i1))(3) + square(a(j1))(1) <= 10)) {
            var index = 0
            val b=new Array[Int](a.length-2)
            for (k <- a.indices if ((k != i1) & (k != j1))) {
              b(index) = a(k); index += 1
            }
            current_pos += a(i1)
            current_pos += a(j1)
            calc_beams(b)
            current_pos -= a(j1)
            current_pos -= a(i1)
          }
          i1 = i1 ^ j1; j1 = i1 ^ j1; i1 = i1 ^ j1
        }
      i1=0
      j1=0
      }
     if(a.length==4){  // third beam the same
        i1=i;j1=j
          for(m<- 0 to 1) {
            if ((square(a(i1))(0) + square(a(j1))(1) + square(current_pos(3))(3) + square(current_pos(2))(2) == 10) &
              (square(current_pos(2))(3) + square(a(i1))(1) + square(current_pos(7))(2) <= 10) &
              (square(current_pos(3))(2) + square(a(j1))(0) <= 10) &
              (square(a(i1))(2) + square(a(j1))(3) <= 10)) {
              var index = 0
              val b=new Array[Int](a.length-2)
              for (k <- a.indices if ((k != i1) & (k != j1))) {
                b(index) = a(k); index += 1
              }
              current_pos += a(i1)
              current_pos += a(j1)
              calc_beams(b)
              current_pos -= a(j1)
              current_pos -= a(i1)
            }
            i1 = i1 ^ j1; j1 = i1 ^ j1; i1 = i1 ^ j1
          }
       i1=0
       j1=0
        }
    if(a.length==2){// fourth beam - if is fit then the figure will completed
      i1=i;j1=j
    for(m<-0 to 1) {
        if ((square(a(i1))(1) + square(a(j1))(3) + square(current_pos(0))(2) + square(current_pos(3))(0) == 10) &
          (square(current_pos(0))(0) + square(a(j1))(1) + square(current_pos(4))(2) <= 10) &
          (square(current_pos(3))(2) + square(a(i1))(3)+square(current_pos(9))(0) <= 10) &
          (square(a(i1))(0) + square(a(j1))(2) <= 10)) {
          current_pos += a(i1)
          current_pos += a(j1)
          count+=1
          print_res(0)// call function for printing result
          println()
          current_pos -= a(j1)
          current_pos -= a(i1)
        }
       i1 = i1 ^ j1; j1 = i1 ^ j1; i1 = i1 ^ j1
      }
      i1=0
      j1=0
    }
      }
  }
}

  def print_res(i:Int): Unit ={    // function prints result
var i1=i
  for(j<-0 to 3) print(square(current_pos(pos(i1)))(j)+" ")
  println()
  i1+=1
  if(i1<12) print_res(i1)
}

  def main(args: Array[String]) {
try {
  val source = Source.fromFile("data.txt") //file must remains in the working directory of program
  val tokens = source.mkString.split("\\s+") //read data to string
  if (tokens.length >= 48) {
    val input = tokens.map(_.toInt)     //transform it into array of numbers
    solution(input)         // call function for solution
  }
  source.close()
}
catch {case es:IOException => {println("File with data not found")}}
  }
}
