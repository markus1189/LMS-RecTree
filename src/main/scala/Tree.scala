package tree

import scala.virtualization.lms.common._

trait Tree { def value: Int }
abstract class Node(value: Int, l: Tree, r: Tree) extends Tree
abstract class Leaf(value: Int) extends Tree

trait TreeOps extends Base with ListOps with Functions {
  object Leaf {
    def apply(i: Rep[Int]): Rep[Tree] = new_leaf(i)
  }
  def new_leaf(i: Rep[Int]): Rep[Tree]

  object Node {
    def apply(l: Rep[Tree], r: Rep[Tree]): Rep[Tree] = new_node(l,r)
  }
  def new_node(l: Rep[Tree], r: Rep[Tree]): Rep[Tree]

  implicit class TreeOperations(t: Rep[Tree]) {
    def kids: Rep[List[Tree]] = tree_kids(t)
    def print: Rep[Unit] = tree_print(t)
  }
  def tree_kids(t: Rep[Tree]): Rep[List[Tree]]
  def tree_print(t: Rep[Tree]): Rep[Unit]
}

trait TreeOpsExp extends TreeOps with BaseExp with ListOpsExp with FunctionsExp {
  case class NewLeaf(i: Exp[Int]) extends Def[Tree]
  def new_leaf(i: Exp[Int]) = NewLeaf(i)

  case class NewNode(l: Exp[Tree], r: Exp[Tree]) extends Def[Tree]
  def new_node(l: Exp[Tree], r: Exp[Tree]) = NewNode(l,r)

  case class TreeKids(t: Exp[Tree]) extends Def[List[Tree]]
  def tree_kids(t: Exp[Tree]): Exp[List[Tree]] = TreeKids(t)

  case class PrintSingle(t: Exp[Tree]) extends Def[Unit]
  def tree_print(t: Exp[Tree]): Exp[Unit] = {
    recursive_print(t)
  }

  private def print_single(t: Exp[Tree]): Exp[Unit] = reflectEffect(PrintSingle(t))

  /** FIXME: recursive call does not work */
  val recursive_print: Exp[Tree => Unit] = doLambda { t: Exp[Tree] => 
      var kids = t.kids
      print_single(t)
      t.kids.map(recursive_print(_))
      unit( {} )
  }
}

trait ScalaGenTree extends ScalaGenBase with ScalaGenFunctions with ScalaGenListOps {
  val IR: TreeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case NewLeaf(i) => emitValDef(sym, "Leaf(" + quote(i) + ")")
    case NewNode(l,r) => emitValDef(sym, "Node(42," + quote(l) + "," + quote(r) + ")")
    case TreeKids(t) => emitValDef(sym, quote(t) + ".kids")
    case PrintSingle(t) => emitValDef(sym, "println(" + quote(t) + ".value" + ")")
    case _ => super.emitNode(sym,node)
  }
}

trait Prog extends TreeOps {
  def f(t: Rep[Tree]): Rep[Unit] = {
    val t1: Rep[Tree] = Node( Node(Leaf(unit(1)),Leaf(unit(2))), Node(Leaf(unit(3)),Leaf(unit(4))))
    t1.print
  }
}

object Main extends App {
  val prog = new Prog with TreeOpsExp
  val codegen = new ScalaGenEffect with ScalaGenTree { val IR: prog.type = prog }
  codegen.emitSource(prog.f, "TreeOps", new java.io.PrintWriter(System.out))
}
