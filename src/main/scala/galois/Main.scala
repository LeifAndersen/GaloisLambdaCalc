package galois

// Language:
// <cexp> ::= (<aexp> <aexp>*)
//         | <aexp>
// <aexp> ::= (λ (<var>*) <cexp>)
//         | <var>
//         | <halt>

sealed trait Exp

sealed trait CExp extends Exp
case class ApplyExp(prog: AExp, arg: List[AExp])extends CExp

sealed trait AExp extends CExp
case class LambExp(param: List[VarExp], body: CExp) extends AExp
case class VarExp(value: String) extends AExp
case class HaltExp() extends AExp

case class Closure(e: Exp, env: Map[VarExp, Address])

sealed trait State
sealed trait EvalState
sealed trait ApplyState
sealed trait HaltState

sealed trait CState
case class CEvalState(e: Exp, env: Map[VarExp, Address], store: CStore)
    extends CState with EvalState
case class CApplyState(f: CEvalState, x: List[CEvalState], store: AStore)
    extends CState with ApplyState
case class CHaltState()
    extends CState with HaltState

sealed trait AState
case class AEvalState(e: Exp, env: Map[VarExp, Address], store: AStore)
    extends AState with EvalState
case class AApplyState(f: AEvalState, x: List[AEvalState], store: AStore)
    extends AState with ApplyState
case class AHaltState()
    extends AState with HaltState

case class Address(address: Exp)

object main extends App {

  def alpha(s: CState): AState = s match {
    case CEvalState(e, env, store) => null
    case CApplyState(f, x, store)  => null
    case CHaltState()              => AHaltState()
  }

  def gamma(s: AState): CState = s match {
    case AEvalState(e, env, store) => null
    case AApplyState(f, x, store)  => null
    case AHaltState()              => CHaltState()
  }
}
