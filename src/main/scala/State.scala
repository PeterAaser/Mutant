package GA

import ST._

object QS {

    def noop[S] = ST[S, Unit](())

    // Create a state transition that mutates S and returns an int indicating pivot location
    def partition[S](arr: STArray[S, Int], left: Int, right: Int, pivot: Int): ST[S,Int] = for {

        // find pivot value, store pivot in the righmost array position
        // and create a reference to the current leftmost swap index (swap candidate index?)
        pivotValue ←  arr.read(pivot)
        _ ←  arr.swap(pivot, right)
        leftmost ←  STRef(left)

        // compose a sort of for loop. remember the signature of leftFold: 
        // fLeft(as: List[A], z: B)(f: (B, A) => B): B, where B is ST[S, Unit], thus 
        //   _ ←  s     desugars to ST[S,Unit] flatMap (_ => ...
        // That is, for each "loop" we want to compose some S => (S, A) ST and sequence them
        // with foldLeft.
        _ ←  (left until right).foldLeft(noop[S])( (s, index) => for {

            _ ←  s
            leftVal ←  arr.read(index)

            // Check if the array element we are iterating over is bigger than the pivot
            _ ←  if (leftVal < pivotValue) (for {

                // If so, swap them, and push the current leftmost swap candidate index
                // one spot to the left.
                vj ←  leftmost.read
                _ ←  arr.swap(index, vj)
                _ ← leftmost.write(vj + 1)

            } yield ()) else noop[S]

        } yield ())

        // After sorting wrt pivot, swap the pivot val which was stored at spot right with the 
        // element at the swap candidate index and return the swap candidate index, which now 
        // holds the index of the sorted value dividing the two sub arrays which will be
        // sorted recursively
        pivotPoint ←  leftmost.read
        _ ←  arr.swap(pivotPoint, right)

    } yield(pivotPoint)


    def qs[S](array: STArray[S,Int], left: Int, right: Int): ST[S, Unit] = if (left < right) {
        val pivotPoint = ((right - left)/2)
        for {
            nextPivot ←  partition(array, left, right, pivotPoint)
            _ ←  qs(array, left, nextPivot - 1)
            _ ←  qs(array, nextPivot + 1, right)
        } yield () 
    } else noop[S]
    
    def quicksort(xs: List[Int]): List[Int] = 
        if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
            def apply[S] = for {
                array ←  STArray.fromList(xs)
                size ←  array.size
                _ ←  qs(array, 0, size - 1)
                sorted ←  array.freeze
            } yield (sorted)
        })
}
