package GA
import reflect.runtime.universe._

sealed trait ST[S, A] { self => 

    protected def run(s: S): (A, S)

    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
        def run(s: S) = {
            val (a, s1) = self.run(s)
            (f(a), s1)
        }
    }
    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
        def run(s: S) = {
            val(a, s1) = self.run(s)
            f(a).run(s1)
        }
    }

}
object ST {
    def apply[S, A](a: => A) = {
        lazy val memo = a
        new ST[S, A] {
            def run(s: S) = (memo, s)
        }
    }
    def runST[A](st: RunnableST[A]): A =
        st.apply[Unit].run( () )._1
}


sealed trait STRef[S, A] {

    protected var cell: A
    def read: ST[S, A] = ST(cell)

    def write(a: A): ST[S, Unit] = new ST[S, Unit] {
        def run(s: S) = {
            cell = a
            ( (), s)
        }
    }
}
object STRef {
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
        var cell = a
    })
}


trait RunnableST[A] {
    def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {

    protected def value: Array[A]
    def size: ST[S, Int] = ST(value.size)

    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
        def run(s: S) = {
            value(i) = a
            ( (), s)
        }
    }

    def fill(xs: Map[Int,A]): ST[S, Unit] =
        xs.foldRight( ST[S, Unit](())) {
            case( (k,v), st ) => st flatMap (_ => write(k, v)) 
        }
    

    def read(i: Int): ST[S, A] = ST(value(i))

    def freeze: ST[S, List[A]] = ST(value.toList)

    def swap(i: Int, j: Int): ST[S, Unit] = for {
        λ1 ←  read(i)
        λ2 ←  read(j)
        _  ←  write(i, λ2)
        _  ←  write(j, λ1)
    } yield ()
}
object STArray {

    def apply[S, A:Manifest](sz: Int, v: A): ST[S, STArray[S, A]] = 
        ST(new STArray[S, A] {
            lazy val value = Array.fill(sz)(v)
        })

    def fromList[S, A:Manifest](xs: List[A]): ST[S, STArray[S,A]] = 
        ST(new STArray[S,A] {
            lazy val value = xs.toArray
        })
}


sealed abstract class STArray2[S, A](implicit manifest: Manifest[A]) {

    protected def value: Array[Array[A]]
    def xSize: ST[S, Int] = ST(value.size)
    def ySize: ST[S, Int] = ST(value(0).size)

    def write(x: Int, y: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
        def run(s: S) = {
            value(x)(y) = a
            ( (), s )
        }
    }

    def read(x: Int, y: Int): ST[S, A] = ST(value(x)(y))

    def readCol(col: Int): ST[S,Vector[A]] = ST(value.map{ _(col - 1) }.toVector)
    def readRow(row: Int): ST[S,Vector[A]] = ST(value(row).toVector)

    def freeze: ST[S, List[List[A]]] = ST(value.map(_.toList).toList)
}
object STArray2 {

    def fromList[S, A:Manifest](xs: List[List[A]]): ST[S, STArray2[S, A]] =
        ST(new STArray2[S,A] {
            lazy val value = xs.map(_.toArray).toArray
        })

    def apply[S, A:Manifest](xDim: Int, yDim: Int, v: A): ST[S, STArray2[S, A]] =
        ST(new STArray2[S,A] {
            lazy val value: Array[Array[A]] = Array.fill(xDim, yDim)(v)
        })
}
