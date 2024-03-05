/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 *
 * All rights reserved.
 *
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.parsing;

import mavlc.errors.SyntaxError;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;

import static mavlc.parsing.Token.TokenType.*;
import static mavlc.syntax.expression.Compare.Comparison.*;

/* TODO enter group information
 *
 * EiCB group number: 57
 * Names and matriculation numbers of all group members:
 * Stefan Nikolaus Dobrea MatrNr. : 2802837
 * Franck Boudouin Tameze MatrNr. : 2682002
 * Narges Ahmadi Asl MatNr. : 2732428
 */

/**
 * A recursive-descent parser for MAVL.
 */
public final class Parser {
	
	private final Deque<Token> tokens;
	private Token currentToken;
	
	/**
	 * @param tokens A token stream that was produced by the {@link Scanner}.
	 */
	public Parser(Deque<Token> tokens) {
		this.tokens = tokens;
		currentToken = tokens.poll();
	}
	
	/**
	 * Parses the MAVL grammar's start symbol, Module.
	 *
	 * @return A {@link Module} node that is the root of the AST representing the tokenized input program.
	 * @throws SyntaxError to indicate that an unexpected token was encountered.
	 */
	public Module parse() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Function> functions = new ArrayList<>();
		List<RecordTypeDeclaration> records = new ArrayList<>();
		while(currentToken.type != EOF) {
			switch(currentToken.type) {
				case FUNCTION:
					functions.add(parseFunction());
					break;
				case RECORD:
					records.add(parseRecordTypeDeclaration());
					break;
				default:
					throw new SyntaxError(currentToken, FUNCTION, RECORD);
			}
		}
		return new Module(location, functions, records);
	}
	
	private String accept(Token.TokenType type) {
		Token t = currentToken;
		if(t.type != type)
			throw new SyntaxError(t, type);
		acceptIt();
		return t.spelling;
	}
	
	private void acceptIt() {
		currentToken = tokens.poll();
		if(currentToken == null || currentToken.type == ERROR)
			throw new SyntaxError(currentToken != null ? currentToken : new Token(EOF, null, -1, -1));
	}
	
	private Function parseFunction() {
		SourceLocation location = currentToken.sourceLocation;

		accept(FUNCTION);
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		List<FormalParameter> parameters = new ArrayList<>();
		List<Statement> body = new ArrayList<>();
		
		accept(LPAREN);
		if(currentToken.type != RPAREN) {
			parameters.add(parseFormalParameter());
			while(currentToken.type != RPAREN) {
				accept(COMMA);
				parameters.add(parseFormalParameter());
			}
		}
		accept(RPAREN);
		
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			body.add(parseStatement());
		accept(RBRACE);
		
		return new Function(location, name, typeSpecifier, parameters, body);
	}
	
	private FormalParameter parseFormalParameter() {
		SourceLocation location = currentToken.sourceLocation;
		
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		
		return new FormalParameter(location, name, typeSpecifier);
	}
	
	private RecordTypeDeclaration parseRecordTypeDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		
		accept(RECORD);
		String name = accept(ID);
		accept(LBRACE);
		List<RecordElementDeclaration> elements = new ArrayList<>();
		// no empty records allowed
		elements.add(parseRecordElementDeclaration());
		while(currentToken.type != RBRACE) {
			elements.add(parseRecordElementDeclaration());
		}
		accept(RBRACE);
		
		return new RecordTypeDeclaration(location, name, elements);
	}
	
	private RecordElementDeclaration parseRecordElementDeclaration() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean isVariable;
		switch(currentToken.type) {
			case VAL:
				acceptIt();
				isVariable = false;
				break;
			case VAR:
				acceptIt();
				isVariable = true;
				break;
			default:
				throw new SyntaxError(currentToken, VAL, VAR);
		}
		
		TypeSpecifier typeSpecifier = parseTypeSpecifier();
		String name = accept(ID);
		accept(SEMICOLON);
		
		return new RecordElementDeclaration(location, isVariable, typeSpecifier, name);
	}
	
	private IteratorDeclaration parseIteratorDeclaration() {
		// TODO implement (task 1.5)
		SourceLocation location = currentToken.sourceLocation;

		boolean isVariable;
		if (currentToken.type == VAL) {
			acceptIt();
			isVariable = false;
		} else if (currentToken.type == VAR) {
			acceptIt();
			isVariable = true;
		} else {
			throw new SyntaxError(currentToken, VAL, VAR);
		}
		TypeSpecifier type = parseTypeSpecifier();
		String name = accept(ID);
		return new IteratorDeclaration(location, name, type, isVariable);
	}
	
	private TypeSpecifier parseTypeSpecifier() {
		SourceLocation location = currentToken.sourceLocation;
		
		boolean vector = false;
		switch(currentToken.type) {
			case INT:
				acceptIt();
				return new IntTypeSpecifier(location);
			case FLOAT:
				acceptIt();
				return new FloatTypeSpecifier(location);
			case BOOL:
				acceptIt();
				return new BoolTypeSpecifier(location);
			case VOID:
				acceptIt();
				return new VoidTypeSpecifier(location);
			case STRING:
				acceptIt();
				return new StringTypeSpecifier(location);
			case VECTOR:
				accept(VECTOR);
				vector = true;
				break;
			case MATRIX:
				accept(MATRIX);
				break;
			case ID:
				String name = accept(ID);
				return new RecordTypeSpecifier(location, name);
			default:
				throw new SyntaxError(currentToken, INT, FLOAT, BOOL, VOID, STRING, VECTOR, MATRIX, ID);
		}
		
		accept(LANGLE);
		TypeSpecifier subtype;
		switch(currentToken.type) {
			case INT:
				subtype = new IntTypeSpecifier(currentToken.sourceLocation);
				break;
			case FLOAT:
				subtype = new FloatTypeSpecifier(currentToken.sourceLocation);
				break;
			default:
				throw new SyntaxError(currentToken, INT, FLOAT);
		}
		acceptIt();
		accept(RANGLE);
		accept(LBRACKET);
		Expression x = parseExpr();
		accept(RBRACKET);
		
		if(vector)
			return new VectorTypeSpecifier(location, subtype, x);
		
		accept(LBRACKET);
		Expression y = parseExpr();
		accept(RBRACKET);
		
		return new MatrixTypeSpecifier(location, subtype, x, y);
	}
	
	private Statement parseStatement() {
		switch(currentToken.type) {
			case VAL:
				return parseValueDef();
			case VAR:
				return parseVarDecl();
			case RETURN:
				return parseReturn();
			case ID:
				return parseAssignOrCall();
			case FOR:
				return parseFor();
			case FOREACH:
				return parseForEach();
			case IF:
				return parseIf();
			case SWITCH:
				return parseSwitch();
			case LBRACE:
				return parseCompound();
			default:
				throw new SyntaxError(currentToken, VAL, VAR, RETURN, ID, FOR, FOREACH, IF, SWITCH, LBRACE);
		}
	}
	
	private ValueDefinition parseValueDef() {
		// TODO implement (task 1.1)
		var location = currentToken.sourceLocation;
		accept(VAL);
		var type = parseTypeSpecifier();
		var id = accept(ID);
		accept(ASSIGN);
		var expr = parseExpr();
		accept(SEMICOLON);

		return new ValueDefinition(location, type, id, expr);
	}
	
	private VariableDeclaration parseVarDecl() {
		// TODO implement (task 1.1)
		var location = currentToken.sourceLocation;
		accept(VAR);
		var type = parseTypeSpecifier();
		var id = accept(ID);
		accept(SEMICOLON);

		return new VariableDeclaration(location, type, id);
	}
	
	private ReturnStatement parseReturn() {
		// TODO implement (task 1.6)
		SourceLocation location = currentToken.sourceLocation;
		accept(RETURN);
		var expression = parseExpr();
		accept(SEMICOLON);
		return new ReturnStatement(location, expression);
	}
	
	private Statement parseAssignOrCall() {
		// TODO implement (task 1.6)
		SourceLocation location = currentToken.sourceLocation;

		String name = accept(ID);

		Statement result;

		if (currentToken.type != LPAREN) {
			result = parseAssign(name,location);
		}

		else {
			result = new CallStatement(location, parseCall(name, location));
		}

		accept(SEMICOLON);
		return result;
	}
	
	private VariableAssignment parseAssign(String name, SourceLocation location) {
		// TODO implement (task 1.1)
		LeftHandIdentifier lhs;

		if (currentToken.type == LBRACKET){
			acceptIt();
			var expr1 = parseExpr();
			accept(RBRACKET);

			if (currentToken.type == LBRACKET){
				accept(LBRACKET);
				var expr2 = parseExpr();
				accept(RBRACKET);
				lhs = new MatrixLhsIdentifier(location, name, expr1, expr2);
			} else {
				lhs = new VectorLhsIdentifier(location, name, expr1);
			}
		} else if (currentToken.type == AT){
			accept(AT);
			var id = accept(ID);
			lhs = new RecordLhsIdentifier(location, name, id);
		} else {
			lhs = new LeftHandIdentifier(location, name);
		}

		accept(ASSIGN);
		return new VariableAssignment(location, lhs, parseExpr());
	}
	
	private CallExpression parseCall(String name, SourceLocation location) {
		// TODO implement (task 1.6)
		List<Expression> parameter = new ArrayList<>();
		accept(LPAREN);
		if(currentToken.type != RPAREN) {
			parameter.add(parseExpr());
			while (currentToken.type != RPAREN) {
				accept(COMMA);
				parameter.add(parseExpr());
			}
		}
		accept(RPAREN);
		return new CallExpression(location, name, parameter);
	}
	
	private ForLoop parseFor() {
		// TODO implement (task 1.4)
		SourceLocation location = currentToken.sourceLocation;
		accept(FOR);
		accept(LPAREN);
		String name = accept(ID);
		accept(ASSIGN);
		Expression a = parseExpr();
		accept(SEMICOLON);
		Expression b = parseExpr();
		accept(SEMICOLON);
		String inc = accept(ID);
		accept(ASSIGN);
		Expression c = parseExpr();
		accept(RPAREN);
		return new ForLoop(location, name, a, b, inc, c, parseStatement());
	}
	
	private ForEachLoop parseForEach() {
		// TODO implement (task 1.5)
		SourceLocation location = currentToken.sourceLocation;
		accept(FOREACH);
		accept(LPAREN);
		IteratorDeclaration iterator = parseIteratorDeclaration();
		accept(COLON);
		Expression struct = parseExpr();
		accept(RPAREN);
		return new ForEachLoop(location, iterator, struct, parseStatement());
	}
	
	private IfStatement parseIf() {
		SourceLocation location = currentToken.sourceLocation;
		accept(IF);
		accept(LPAREN);
		Expression test = parseExpr();
		accept(RPAREN);
		Statement then = parseStatement();
		if(currentToken.type == ELSE) {
			acceptIt();
			return new IfStatement(location, test, then, parseStatement());
		}
		return new IfStatement(location, test, then);
	}
	
	private SwitchStatement parseSwitch() {
		// TODO implement (task 1.7)
		SourceLocation location = currentToken.sourceLocation;

		List<Case> cases = new ArrayList<>();
		List<Default> defaults = new ArrayList<>();

		accept(SWITCH);
		accept(LPAREN);
		Expression expression = parseExpr();
		accept(RPAREN);
		accept(LBRACE);

		while (currentToken.type != RBRACE) {
			switch (currentToken.type) {
				case CASE:
					cases.add(parseCase());
					break;
				case DEFAULT:
					defaults.add(parseDefault());
					break;
			}
		}
		acceptIt();
		return new SwitchStatement(location, expression, cases, defaults);
	}
	
	private Case parseCase() {
		// TODO implement (task 1.7)
		SourceLocation location = currentToken.sourceLocation;

		accept(CASE);
		Expression expression = parseExpr();
		accept(COLON);
		Statement statement = parseStatement();
		return new Case(location, expression, statement);
	}
	
	private Default parseDefault() {
		// TODO implement (task 1.7)
		SourceLocation location = currentToken.sourceLocation;

		accept(DEFAULT);
		accept(COLON);
		Statement statement = parseStatement();

		return new Default(location, statement);
	}
	
	private CompoundStatement parseCompound() {
		SourceLocation location = currentToken.sourceLocation;
		
		List<Statement> statements = new ArrayList<>();
		accept(LBRACE);
		while(currentToken.type != RBRACE)
			statements.add(parseStatement());
		accept(RBRACE);
		
		return new CompoundStatement(location, statements);
	}
	
	private Expression parseExpr() {
		return parseSelect();
	}
	
	private Expression parseSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression cond = parseOr();
		if(currentToken.type == QMARK) {
			acceptIt();
			Expression trueCase = parseOr();
			accept(COLON);
			Expression falseCase = parseOr();
			return new SelectExpression(location, cond, trueCase, falseCase);
		}
		return cond;
	}
	
	private Expression parseOr() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAnd();
		while(currentToken.type == OR) {
			acceptIt();
			x = new Or(location, x, parseAnd());
		}
		return x;
	}
	
	private Expression parseAnd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseNot();
		while(currentToken.type == AND) {
			acceptIt();
			x = new And(location, x, parseNot());
		}
		return x;
	}
	
	private Expression parseNot() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == NOT) {
			acceptIt();
			// TODO replace null by call to the appropriate parse method (task 1.2)
			return new Not(location, parseCompare());
		}
		// TODO replace null by call to the appropriate parse method (task 1.2)
		return parseCompare();
	}
	
	private Expression parseCompare() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;

		Expression expression = parseAddSub();
		Compare.Comparison type;
		out:
		while (true) {
			// find out what type of comparison this is.
			switch (currentToken.type) {
				case RANGLE:
					type = GREATER;
					break;
				case LANGLE:
					type = LESS;
					break;
				case CMPLE:
					type = LESS_EQUAL;
					break;
				case CMPGE:
					type = GREATER_EQUAL;
					break;
				case CMPEQ:
					type = EQUAL;
					break;
				case CMPNE:
					type = NOT_EQUAL;
					break;
				// well, shit, if this isn't a comparison at all we're done.
				default:
					break out;
			}

			// this is a comparison of `type`, so we update the expression.
			acceptIt();
			expression = new Compare(location, expression, parseAddSub(), type);
		}

		return expression;
	}
	
	private Expression parseAddSub() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;

		Expression expression = parseMulDiv();

		out:
		while (true) {
			switch (currentToken.type) {
				case ADD:
					acceptIt();
					expression = new Addition(location, expression, parseMulDiv());
					break;
				case SUB:
					acceptIt();
					expression = new Subtraction(location, expression, parseMulDiv());
					break;
				default:
					break out;
			}
		}

		return expression;
	}
	
	private Expression parseMulDiv() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;

		Expression expression = parseUnaryMinus();

		out:
		while (true) {
			switch (currentToken.type) {
				case MULT:
					acceptIt();
					expression = new Multiplication(location, expression, parseUnaryMinus());
					break;
				case DIV:
					acceptIt();
					expression = new Division(location, expression, parseUnaryMinus());
					break;
				default:
					break out;
			}
		}

		return expression;
	}
	
	private Expression parseUnaryMinus() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == SUB) {
			acceptIt();
			// TODO replace null by call to the appropriate parse method (task 1.2)
			return new UnaryMinus(location, parseExponentiation());
		}
		// TODO replace null by call to the appropriate parse method (task 1.2)
		return parseExponentiation();
	}
	
	private Expression parseExponentiation() {
		// TODO implement (task 1.2)
		SourceLocation location = currentToken.sourceLocation;

		Expression base = parseDotProd();

		while(currentToken.type == EXP){
			acceptIt();
			base = new Exponentiation(location, base, parseDotProd());
		}

		return base;
	}
	
	private Expression parseDotProd() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseMatrixMul();
		while(currentToken.type == DOTPROD) {
			acceptIt();
			x = new DotProduct(location, x, parseMatrixMul());
		}
		return x;
	}
	
	private Expression parseMatrixMul() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseTranspose();
		while(currentToken.type == MATMULT) {
			acceptIt();
			x = new MatrixMultiplication(location, x, parseTranspose());
		}
		return x;
	}
	
	private Expression parseTranspose() {
		SourceLocation location = currentToken.sourceLocation;
		
		if(currentToken.type == TRANSPOSE) {
			acceptIt();
			// TODO replace null by call to the appropriate parse method (task 1.2)
			 return new MatrixTranspose(location, parseDim());
		}
		// TODO replace null by call to the appropriate parse method (task 1.2)
		return parseDim();
	}
	
	private Expression parseDim() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseSubRange();
		switch(currentToken.type) {
			case ROWS:
				acceptIt();
				return new MatrixRows(location, x);
			case COLS:
				acceptIt();
				return new MatrixCols(location, x);
			case DIM:
				acceptIt();
				return new VectorDimension(location, x);
			default:
				return x;
		}
	}
	
	private Expression parseSubRange() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseElementSelect();
		
		if(currentToken.type == LBRACE) {
			acceptIt();
			Expression xStartIndex = parseExpr();
			accept(COLON);
			Expression xBaseIndex = parseExpr();
			accept(COLON);
			Expression xEndIndex = parseExpr();
			accept(RBRACE);
			if(currentToken.type != LBRACE)
				return new SubVector(location, x, xBaseIndex, xStartIndex, xEndIndex);
			
			accept(LBRACE);
			Expression yStartIndex = parseExpr();
			accept(COLON);
			Expression yBaseIndex = parseExpr();
			accept(COLON);
			Expression yEndIndex = parseExpr();
			accept(RBRACE);
			return new SubMatrix(location, x, xBaseIndex, xStartIndex, xEndIndex, yBaseIndex, yStartIndex, yEndIndex);
		}
		
		return x;
	}
	
	private Expression parseElementSelect() {
		// TODO implement (task 1.3)
		SourceLocation location = currentToken.sourceLocation;

		Expression expression = parseRecordElementSelect();

		while (currentToken.type == LBRACKET) {
			acceptIt();
			Expression index = parseExpr();
			accept(RBRACKET);
			expression = new ElementSelect(location, expression, index);
		}

		return expression;
	}
	
	private Expression parseRecordElementSelect() {
		SourceLocation location = currentToken.sourceLocation;
		
		Expression x = parseAtom();
		
		if(currentToken.type == AT) {
			accept(AT);
			String elementName = accept(ID);
			x = new RecordElementSelect(location, x, elementName);
		}
		
		return x;
	}
	
	private Expression parseAtom() {
		SourceLocation location = currentToken.sourceLocation;
		
		switch(currentToken.type) {
			case INTLIT:
				return new IntValue(location, parseIntLit());
			case FLOATLIT:
				return new FloatValue(location, parseFloatLit());
			case BOOLLIT:
				return new BoolValue(location, parseBoolLit());
			case STRINGLIT:
				return new StringValue(location, accept(STRINGLIT));
			default: /* check other cases below */
		}
		
		if(currentToken.type == ID) {
			String name = accept(ID);
			if(currentToken.type != LPAREN) {
				return new IdentifierReference(location, name);
				
			} else {
				return parseCall(name, location);
			}
		}
		
		if(currentToken.type == LPAREN) {
			acceptIt();
			Expression x = parseExpr();
			accept(RPAREN);
			return x;
		}
		
		if(currentToken.type == AT) {
			acceptIt();
			String name = accept(ID);
			return new RecordInit(location, name, parseInitializerList());
		}
		
		if(currentToken.type == LBRACKET) {
			return new StructureInit(location, parseInitializerList());
		}
		
		throw new SyntaxError(currentToken, INTLIT, FLOATLIT, BOOLLIT, STRINGLIT, ID, LPAREN, LBRACKET, AT);
	}
	
	private List<Expression> parseInitializerList() {
		List<Expression> elements = new ArrayList<>();
		
		accept(LBRACKET);
		elements.add(parseExpr());
		while(currentToken.type == COMMA) {
			accept(COMMA);
			elements.add(parseExpr());
		}
		accept(RBRACKET);
		
		return elements;
	}
	
	private int parseIntLit() {
		return Integer.parseInt(accept(INTLIT));
	}
	
	private float parseFloatLit() {
		return Float.parseFloat(accept(FLOATLIT));
	}
	
	private boolean parseBoolLit() {
		return Boolean.parseBoolean(accept(BOOLLIT));
	}
}
