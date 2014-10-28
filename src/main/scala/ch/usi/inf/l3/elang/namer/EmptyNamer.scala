package ch.usi.inf.l3.elang.namer

// trait Tpe
// case object Error extends Type
// case class ATpe(str: String) extends Type
//
// class Types private (private val store: List[String] = Nil){
//   def put(s: String): Types = new Types(s :: store)
//   def get(s: String): Type = store.contains(s) match {
//     case false => Error
//     case _ => ATpe(s)
//   }
//   def has(s: String): Boolean = store.contains(s)
// }



trait Type
case object ErrorType extends Type
case object NoType extends Type

trait TType extends Type {
  def isSubtypeOf(other: TType)(implicit context: SymbolTable): Boolean
  def vars: List[VarType]
  def methods: List[MethodType]
  def field(name: String)(implicit context: SymbolTable): Option[VarType]
  def method(name: String)(implicit context: SymbolTable): Option[MethodType]
}

case object ObjectType extends TType {
  def isSubtypeOf(other: TType)(implicit context: SymbolTable): Boolean = 
    other == this
  def vars: List[VarType] = Nil
  def methods: List[MethodType] = Nil
  def field(name: String)(implicit context: SymbolTable): Option[VarType] = 
    None
  def method(name: String)(implicit context: SymbolTable): Option[MethodType] = 
    None
}

case class ClassType(name: String, parent: String, vars: List[VarType], 
                     methods: List[MethodType]) extends TType {
  def isSubtypeOf(other: TType)(implicit context: 
                  SymbolTable): Boolean = {
    context.get(parent) match {
      case Some(p) => 
        if(other == this || other == p || p == ObjectType) true
        else {
          p.isSubtypeOf(other) 
        }
      case None => false
    }
  }

  def field(name: String)(implicit context: 
              SymbolTable): Option[VarType] = {
    vars.filter(_.name == name).headOption match {
      case None => context.get(parent) match {
        case Some(p) => p.field(name)
        case _ => None
      }
      case m => m
    }
  }

  def method(name: String)(implicit context:
              SymbolTable): Option[MethodType] = {
    methods.filter(_.name == name).headOption match {
      case None => context.get(parent) match {
        case Some(p) => p.method(name)
        case _ => None
      }
      case m => m
    }
  }
}

case class VarType(name: String, tpe: String) extends Type
case class MethodType(ret: String, name: String, 
        params: List[VarType]) extends Type {
  def field(name: String): Option[VarType] = {
    params.filter(_.name == name).headOption 
  }
}

case class SymbolTable private (private val context: 
        Map[String, TType] = Map("Object" -> ObjectType)){
  def put(nme: String, tpe: TType): SymbolTable = {
    SymbolTable(context + (nme -> tpe))
  }
  def get(nme: String): Option[TType] = context.get(nme)
  def defines(nme: String): Boolean = context.contains(nme)
}

trait Namer {
  def name(st: SymbolTable): (SymbolTable, Type)
  val treeName: String
  def treeType(st: SymbolTable): Type
}
