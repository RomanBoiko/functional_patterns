abstract class Expr {
	public <T> T accept(ExprVisitor<T> visitor) {
		visitor.visit(this);
	}
}

class Num extends Expr {
	private int value;

	public Num(int value) {
		this.value = value;
	}

	public int getValue() {
		return value;
	}
}

abstract class BiCompositeExpr extends Expr {
	private Expr left;
	private Expr right;

	protected BiCompositeExpr(Expr left, Expr right) {
		this.left = left;
		this.right = right;
	}

	public Expr getLeft() {
		return left;
	}

	public Expr getRight() {
		return right;
	}
}

class Sum extends BiCompositeExpr {
	protected Sum(Expr left, Expr right) {
		super(left, right);
	}
}

class Prod extends BiCompositeExpr {
	protected Prod(Expr left, Expr right) {
		super(left, right);
	}
}

interface ExprVisitor<T> {
	T visit(Num num);

	T visit(Sum sum);

	T visit(Prod prod);
}

class EvalExpr implements ExprVisitor<Integer> {
	public Integer visit(Num num) {
		return num.getValue();
	}

	public Integer visit(Sum sum) {
		return sum.getLeft().accept(this) + sum.getRight().accept(this);
	}

	public Integer visit(Prod prod) {
		return prod.getLeft().accept(this) * prod.getRight().accept(this);
	}
}

class PrintExpr implements ExprVisitor<Void> {
	public Void visit(Num num) {
		print(" " + num.getValue() + " ");
		return null;
	}

	public Void visit(Sum sum) {
		sum.getLeft().accept(this);
		print("+");
		sum.getRight().accept(this);
		return null;
	}

	public Void visit(Prod prod) {
		prod.getLeft().accept(this);
		print("*");
		prod.getRight().accept(this);
		return null;
	}
}