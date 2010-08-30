//
// Generated by JTB 1.1.2
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> PrimaryExpression()
 * f1 -> [ "++" | "--" ]
 * </PRE>
 */
public class PostfixExpression implements Node {
   public PrimaryExpression f0;
   public NodeOptional f1;

   public PostfixExpression(PrimaryExpression n0, NodeOptional n1) {
      f0 = n0;
      f1 = n1;
   }

   public void accept(jde.parser.visitor.Visitor v) {
      v.visit(this);
   }
}

