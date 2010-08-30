//
// Generated by JTB 1.1.2
//

package jde.parser.syntaxtree;

/**
 * Grammar production:
 * <PRE>
 * f0 -> "import"
 * f1 -> Name()
 * f2 -> [ "." "*" ]
 * f3 -> ";"
 * </PRE>
 */
public class ImportDeclaration implements Node {
   public NodeToken f0;
   public Name f1;
   public NodeOptional f2;
   public NodeToken f3;

   public ImportDeclaration(NodeToken n0, Name n1, NodeOptional n2, NodeToken n3) {
      f0 = n0;
      f1 = n1;
      f2 = n2;
      f3 = n3;
   }

   public ImportDeclaration(Name n0, NodeOptional n1) {
      f0 = new NodeToken("import");
      f1 = n0;
      f2 = n1;
      f3 = new NodeToken(";");
   }

   public void accept(jde.parser.visitor.Visitor v) {
      v.visit(this);
   }
}
