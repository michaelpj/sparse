package sparse

import org.kiama.output.PrettyPrinter
import OptionParsers._

object Printer extends PrettyPrinter {
  
  def show(n: Name): Doc = n match {
    case Long(n) => "--" <> n
    case Short(n) => "-" <> n.toString
    case Plain(n) => n
  }
    
  def show[T](o: Opt[T]): Doc = o match {
    case OptAndArgs(p,d,pa) => pa.names.headOption.map(show).map(_ <+> pa.metavar) getOrElse empty
    case Arg(p) => "ARG"
  }
  
  def isOptional[T](o: Opt[T]): Boolean = o match {
    case OptAndArgs(p,d,pa) => d.isDefined
    case Arg(p) => false
  }
  
  def usage[T](p: Prsr[T]): Doc = p match {
    case Done(v) => empty
    case OptP(o) => {
      val f = show(o)
      if (isOptional(o)) brackets(f) else f
    }
    case Alt(l,r) => "alt" <> parens(usage(l) <+> usage(r))
    case Chain(opt, rest) => usage(rest) <+> usage(opt)
    case Bind(arg, bound) => usage(arg) <+> "..."
  }
  
  def help[T](p: Prsr[T]): Doc = p match {
    case Done(v) => empty
    case OptP(o) => o match {
      case OptAndArgs(p, d, pa) => pa.doc.map(show(o) <@> _) getOrElse empty
      case Arg(p) => ""
    } 
    case Alt(l,r) => help(l) <+> "|" <+> help(r)
    case Chain(opt, rest) => help(rest) <@> help(opt)
    case Bind(arg, bound) => help(arg) <+> "..."
  }
  
  def allHelp[T](p: Prsr[T]): Doc = "Usage:" <+> usage(p) <@> "Options: " <+> help(p)
  
}