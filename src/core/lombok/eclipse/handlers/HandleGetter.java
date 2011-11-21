/*
 * Copyright (C) 2009-2011 The Project Lombok Authors.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package lombok.eclipse.handlers;

import static lombok.eclipse.Eclipse.*;
import static lombok.eclipse.handlers.EclipseHandlerUtil.*;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.core.AST.Kind;
import lombok.core.AnnotationValues;
import lombok.core.TransformationsUtil;
import lombok.eclipse.EclipseAnnotationHandler;
import lombok.eclipse.EclipseNode;

import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.compiler.IProblemFactory;
import org.eclipse.jdt.internal.compiler.ISourceElementRequestor;
import org.eclipse.jdt.internal.compiler.SourceElementParser;
import org.eclipse.jdt.internal.compiler.SourceElementRequestorAdapter;
import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.AllocationExpression;
import org.eclipse.jdt.internal.compiler.ast.Annotation;
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference;
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.ast.FieldDeclaration;
import org.eclipse.jdt.internal.compiler.ast.MethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.ParameterizedQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeReference;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jdt.internal.compiler.problem.DefaultProblemFactory;
import org.mangosdk.spi.ProviderFor;

/**
 * Handles the {@code lombok.Getter} annotation for eclipse.
 */
@ProviderFor(EclipseAnnotationHandler.class) public class HandleGetter extends EclipseAnnotationHandler<Getter> {
	public boolean generateGetterForType(EclipseNode typeNode, EclipseNode pos, AccessLevel level, boolean checkForTypeLevelGetter) {
		if (checkForTypeLevelGetter) {
			if (typeNode != null) for (EclipseNode child : typeNode.down()) {
				if (child.getKind() == Kind.ANNOTATION) {
					if (annotationTypeMatches(Getter.class, child)) {
						// The annotation will make it happen, so we can skip
						// it.
						return true;
					}
				}
			}
		}
		
		TypeDeclaration typeDecl = null;
		if (typeNode.get() instanceof TypeDeclaration) typeDecl = (TypeDeclaration) typeNode.get();
		int modifiers = typeDecl == null ? 0 : typeDecl.modifiers;
		boolean notAClass = (modifiers & (ClassFileConstants.AccInterface | ClassFileConstants.AccAnnotation)) != 0;
		
		if (typeDecl == null || notAClass) {
			pos.addError("@Getter is only supported on a class, an enum, or a field.");
			return false;
		}
		
		for (EclipseNode field : typeNode.down()) {
			if (fieldQualifiesForGetterGeneration(field)) generateGetterForField(field, pos.get(), level, false);
		}
		return true;
	}
	
	public boolean fieldQualifiesForGetterGeneration(EclipseNode field) {
		if (field.getKind() != Kind.FIELD) return false;
		FieldDeclaration fieldDecl = (FieldDeclaration) field.get();
		return filterField(fieldDecl);
	}
	
	/**
	 * Generates a getter on the stated field.
	 * 
	 * Used by {@link HandleData}.
	 * 
	 * The difference between this call and the handle method is as follows:
	 * 
	 * If there is a {@code lombok.Getter} annotation on the field, it is used
	 * and the same rules apply (e.g. warning if the method already exists,
	 * stated access level applies). If not, the getter is still generated if it
	 * isn't already there, though there will not be a warning if its already
	 * there. The default access level is used.
	 */
	public void generateGetterForField(EclipseNode fieldNode, ASTNode pos, AccessLevel level, boolean lazy) {
		for (EclipseNode child : fieldNode.down()) {
			if (child.getKind() == Kind.ANNOTATION) {
				if (annotationTypeMatches(Getter.class, child)) {
					// The annotation will make it happen, so we can skip it.
					return;
				}
			}
		}
		
		createGetterForField(level, fieldNode, fieldNode, pos, false, lazy);
	}
	
	public void handle(AnnotationValues<Getter> annotation, Annotation ast, EclipseNode annotationNode) {
		EclipseNode node = annotationNode.up();
		Getter annotationInstance = annotation.getInstance();
		AccessLevel level = annotationInstance.value();
		boolean lazy = annotationInstance.lazy();
		if (level == AccessLevel.NONE) {
			if (lazy) {
				annotationNode.addWarning("'lazy' does not work with AccessLevel.NONE.");
			}
			return;
		}
		
		if (node == null) return;
		
		switch (node.getKind()) {
		case FIELD:
			createGetterForFields(level, annotationNode.upFromAnnotationToFields(), annotationNode, annotationNode.get(), true, lazy);
			break;
		case TYPE:
			if (lazy) annotationNode.addError("'lazy' is not supported for @Getter on a type.");
			generateGetterForType(node, annotationNode, level, false);
			break;
		}
	}
	
	private void createGetterForFields(AccessLevel level, Collection<EclipseNode> fieldNodes, EclipseNode errorNode, ASTNode source, boolean whineIfExists, boolean lazy) {
		for (EclipseNode fieldNode : fieldNodes) {
			createGetterForField(level, fieldNode, errorNode, source, whineIfExists, lazy);
		}
	}
	
	private void createGetterForField(AccessLevel level, EclipseNode fieldNode, EclipseNode errorNode, ASTNode source, boolean whineIfExists, boolean lazy) {
		if (fieldNode.getKind() != Kind.FIELD) {
			errorNode.addError("@Getter is only supported on a class or a field.");
			return;
		}
		
		FieldDeclaration field = (FieldDeclaration) fieldNode.get();
		if (lazy) {
			if ((field.modifiers & ClassFileConstants.AccPrivate) == 0 || (field.modifiers & ClassFileConstants.AccFinal) == 0) {
				errorNode.addError("'lazy' requires the field to be private and final.");
				return;
			}
			if (field.initialization == null) {
				errorNode.addError("'lazy' requires field initialization.");
				return;
			}
		}
		
		TypeReference fieldType = copyType(field.type, source);
		String fieldName = new String(field.name);
		boolean isBoolean = nameEquals(fieldType.getTypeName(), "boolean") && fieldType.dimensions() == 0;
		String getterName = TransformationsUtil.toGetterName(fieldName, isBoolean);
		
		int modifier = toEclipseModifier(level) | (field.modifiers & ClassFileConstants.AccStatic);
		
		for (String altName : TransformationsUtil.toAllGetterNames(fieldName, isBoolean)) {
			switch (methodExists(altName, fieldNode, false)) {
			case EXISTS_BY_LOMBOK:
				return;
			case EXISTS_BY_USER:
				if (whineIfExists) {
					String altNameExpl = "";
					if (!altName.equals(getterName)) altNameExpl = String.format(" (%s)", altName);
					errorNode.addWarning(String.format("Not generating %s(): A method with that name already exists%s", getterName, altNameExpl));
				}
				return;
			default:
			case NOT_EXISTS:
				// continue scanning the other alt names.
			}
		}
		
		MethodDeclaration method = generateGetter((TypeDeclaration) fieldNode.up().get(), fieldNode, getterName, modifier, source, lazy);
		Annotation[] copiedAnnotations = copyAnnotations(source, findAnnotations(field, TransformationsUtil.NON_NULL_PATTERN), findAnnotations(field, TransformationsUtil.NULLABLE_PATTERN));
		if (copiedAnnotations.length != 0) {
			method.annotations = copiedAnnotations;
		}
		
		injectMethod(fieldNode.up(), method);
	}
	
	private static final String AR = "java.util.concurrent.atomic.AtomicReference";
	private static final TypeReference[][] AR_PARAMS = new TypeReference[5][];
	
	private static final java.util.Map<String, char[][]> TYPE_MAP;
	static {
		Map<String, char[][]> m = new HashMap<String, char[][]>();
		m.put("int", fromQualifiedName("java.lang.Integer"));
		m.put("double", fromQualifiedName("java.lang.Double"));
		m.put("float", fromQualifiedName("java.lang.Float"));
		m.put("short", fromQualifiedName("java.lang.Short"));
		m.put("byte", fromQualifiedName("java.lang.Byte"));
		m.put("long", fromQualifiedName("java.lang.Long"));
		m.put("boolean", fromQualifiedName("java.lang.Boolean"));
		m.put("char", fromQualifiedName("java.lang.Character"));
		TYPE_MAP = Collections.unmodifiableMap(m);
	}
	
	
	private final String TAG_GETTERACCESS = "<<GETTER_ACCESS>>";
	private final String TAG_GETTERMODIFIER = "<<GETTER_ACCESSMODIFIER>>";
	private final String TAG_GETTERSTATIC = "<<GETTER_STATIC>>";
	private final String TAG_GETTERTYPE = "<<GETTER_TYPE>>";
	private final String TAG_GETTERNAME = "<<GETTER_NAME>>";
	private final String TAG_GETTERFIELD = "<<GETTER_FIELD>>";
	private final String GETTER_TEMPLATE = 
		TAG_GETTERMODIFIER+TAG_GETTERSTATIC+TAG_GETTERTYPE + " " + TAG_GETTERNAME + "(){ " +
			"return "+TAG_GETTERACCESS+TAG_GETTERFIELD + ";" +
		"}";
	
	private final String TAG_GETTER_INITIALVALUE = "<<GETTER_INITIALVALUE>>";
	private final String TAG_GETTER_OBJECTTYPE = "<<GETTER_OBJECTTYPE>>";
	private final String LAZY_GETTER_TEMPLATE =
		TAG_GETTERMODIFIER+TAG_GETTERSTATIC+TAG_GETTERTYPE + " " + TAG_GETTERNAME + "(){"+
		 	AR+"<"+TAG_GETTER_OBJECTTYPE+"> value="+TAG_GETTERACCESS+TAG_GETTERFIELD+".get();"+
		 	"if (value == null) {"+
		 		"synchronized("+TAG_GETTERACCESS+TAG_GETTERFIELD+") {"+
		 			"value="+TAG_GETTERACCESS+TAG_GETTERFIELD+".get();"+
		 			"if (value == null) {"+
		 				"value=new "+AR+"<"+TAG_GETTER_OBJECTTYPE+">("+TAG_GETTER_INITIALVALUE+");"+
		 				TAG_GETTERACCESS+TAG_GETTERFIELD+".set(value);"+
		 			"}"+
		 		"}"+
		 	"}"+
		 	"return value.get();"+
		 "}";

	
	private MethodDeclaration generateGetter(TypeDeclaration parent, EclipseNode fieldNode, String name, int modifier, ASTNode source, boolean lazy) {
		// Remember the type; lazy will change it;
		FieldDeclaration field = ((FieldDeclaration)fieldNode.get());
		String returnTypeString = String.valueOf(((FieldDeclaration) fieldNode.get()).type.getLastToken());
		
		final String accessorModifier = getAccessModifierString(modifier);
		boolean isStatic = hasStaticAccessor(field);
		final String accessor = getAccessorName(fieldNode, isStatic);
		final String accessorStatic = (isStatic ? "static " : "");
		
		final String getterSrc;
		if (lazy) {
			getterSrc = LAZY_GETTER_TEMPLATE
							.replaceAll(TAG_GETTERMODIFIER, accessorModifier)
							.replaceAll(TAG_GETTERSTATIC, accessorStatic)
							.replaceAll(TAG_GETTERNAME, name)
							.replaceAll(TAG_GETTERACCESS, accessor)
							.replaceAll(TAG_GETTERFIELD, fieldNode.getName())
							.replaceAll(TAG_GETTERTYPE, returnTypeString)
							.replaceAll(TAG_GETTER_OBJECTTYPE, toQualifiedName(TYPE_MAP.get(returnTypeString)))
							.replaceAll(TAG_GETTER_INITIALVALUE, field.initialization.printExpression(0, new StringBuffer()).toString());
		
			changeFieldType(source, field);
		} else {
			getterSrc = GETTER_TEMPLATE
					.replaceAll(TAG_GETTERMODIFIER, accessorModifier)
					.replaceAll(TAG_GETTERSTATIC, accessorStatic)
					.replaceAll(TAG_GETTERNAME, name)
					.replaceAll(TAG_GETTERACCESS, accessor)
					.replaceAll(TAG_GETTERFIELD, fieldNode.getName())
					.replaceAll(TAG_GETTERTYPE, returnTypeString);
		}
		
		MethodDeclaration method = parseMethod(parent, getterSrc);
//		int newMethodSize = method.declarationSourceEnd-method.declarationSourceStart+1;
		int newOffset = parent.bodyStart;
//		parent.bodyEnd += newMethodSize;
//		parent.declarationSourceEnd += newMethodSize;
		method.traverse(new MarkAsGeneratedByVisitor(source, newOffset), parent.scope);

		return method;
	}

	private MethodDeclaration parseMethod(TypeDeclaration parent, final String getterSrc) {
		ISourceElementRequestor requestor = new SourceElementRequestorAdapter();
		IProblemFactory problemFactory = new DefaultProblemFactory();
		CompilerOptions options = new CompilerOptions(JavaCore.getOptions());
		SourceElementParser parser = new SourceElementParser(requestor, problemFactory, options, false, false);

		CompilationUnitDeclaration sourceUnit = new CompilationUnitDeclaration(parser.problemReporter(), parent.compilationResult, getterSrc.length());
		ASTNode[] decl = parser.parseClassBodyDeclarations(getterSrc.toCharArray(), 0, getterSrc.length(), sourceUnit);
		MethodDeclaration method = (MethodDeclaration) decl[0];
		return method;
	}

	private String getAccessorName(EclipseNode fieldNode, boolean isStatic) {
		final String accessor;
		if (isStatic) {
			EclipseNode containerNode = fieldNode.up();
			if (containerNode != null && containerNode.get() instanceof TypeDeclaration) {
				accessor = String.valueOf(((TypeDeclaration)containerNode.get()).name)+".";
			} else {
				accessor = "";
			}
		} else {
			accessor = "this.";
		}
		return accessor;
	}

	private boolean hasStaticAccessor(FieldDeclaration field) {
		return (field.modifiers & ClassFileConstants.AccStatic) != 0;
	}

	private String getAccessModifierString(int modifier) {
		final String accessorModifier;
		if ((modifier & ClassFileConstants.AccPrivate) != 0) {
			accessorModifier = "";
		} else if ((modifier & ClassFileConstants.AccProtected) != 0) {
			accessorModifier = "protected ";
		} else if ((modifier & ClassFileConstants.AccPublic) != 0) {
			accessorModifier = "public ";			
		} else {
			throw new IllegalArgumentException();
		}
		return accessorModifier;
	}

	private void changeFieldType(ASTNode source, FieldDeclaration field) {
		{
			/*
			 * @Getter(lazy=true,value=AccessLevel.PRIVATE) 
			 * private final java.util.concurrent.atomic.AtomicReference<java.util.concurrent.atomic.AtomicReference<java.lang.Integer>> doSomething=new java.util.concurrent.atomic.AtomicReference<java.util.concurrent.atomic.AtomicReference<java.lang.Integer>>();
			 * 
			 */
			TypeReference componentType = copyType(field.type, source);
			if (field.type instanceof SingleTypeReference && !(field.type instanceof ArrayTypeReference)) {
				char[][] newType = TYPE_MAP.get(new String(((SingleTypeReference) field.type).token));
				if (newType != null) {
					componentType = new QualifiedTypeReference(newType, poss(source, 3));
					setGeneratedBy(componentType, source);
				}
			}

			TypeReference[][] firstTypeParams = AR_PARAMS.clone();
			firstTypeParams[4] = new TypeReference[] {copyType(componentType, source)};
			TypeReference firstType = new ParameterizedQualifiedTypeReference(fromQualifiedName(AR), firstTypeParams, 0, poss(source, 5));
			
			TypeReference innerType = copyType(firstType, source);
			
			TypeReference[][] typeParams = AR_PARAMS.clone();
			typeParams[4] = new TypeReference[] {copyType(innerType, source)};
			TypeReference type = new ParameterizedQualifiedTypeReference(fromQualifiedName(AR), typeParams, 0, poss(source, 5));
			// Some magic here
			type.sourceStart = -1;
			type.sourceEnd = type.statementEnd = -2;
			setGeneratedBy(type, source);
			
			field.type = type;
			AllocationExpression init = new AllocationExpression();
			// Some magic here
			init.sourceStart = field.initialization.sourceStart;
			init.sourceEnd = init.statementEnd = field.initialization.sourceEnd;
			init.type = copyType(type, source);
			field.initialization = init;
		}
	}
	
//	private Statement[] createSimpleGetterBody(ASTNode source, EclipseNode fieldNode) {
//		FieldDeclaration field = (FieldDeclaration) fieldNode.get();
//		Expression fieldRef = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source);
//		Statement returnStatement = new ReturnStatement(fieldRef, field.sourceStart, field.sourceEnd);
//		setGeneratedBy(returnStatement, source);
//		return new Statement[] {returnStatement};
//	}
	
//	private static char[] valueName = "value".toCharArray();
	
//	private Statement[] createLazyGetterBody(ASTNode source, EclipseNode fieldNode) {
//		/*
//		 * java.util.concurrent.atomic.AtomicReference<ValueType> value =
//		 * this.fieldName.get(); if (value == null) { synchronized
//		 * (this.fieldName) { value = this.fieldName.get(); if (value == null) {
//		 * value = new
//		 * java.util.concurrent.atomic.AtomicReference<ValueType>(new
//		 * ValueType()); this.fieldName.set(value); } } } return value.get();
//		 */
//		
//		FieldDeclaration field = (FieldDeclaration) fieldNode.get();
//		int pS = source.sourceStart, pE = source.sourceEnd;
//		long p = (long) pS << 32 | pE;
//		
//		TypeReference componentType = copyType(field.type, source);
//		if (field.type instanceof SingleTypeReference && !(field.type instanceof ArrayTypeReference)) {
//			char[][] newType = TYPE_MAP.get(new String(((SingleTypeReference) field.type).token));
//			if (newType != null) {
//				componentType = new QualifiedTypeReference(newType, poss(source, 3));
//				setGeneratedBy(componentType, source);
//			}
//		}
//		
//		Statement[] statements = new Statement[3];
//		
//		/*
//		 * java.util.concurrent.atomic.AtomicReference<ValueType> value =
//		 * this.fieldName.get();
//		 */{
//			LocalDeclaration valueDecl = new LocalDeclaration(valueName, pS, pE);
//			setGeneratedBy(valueDecl, source);
//			TypeReference[][] typeParams = AR_PARAMS.clone();
//			typeParams[4] = new TypeReference[] {copyType(componentType, source)};
//			valueDecl.type = new ParameterizedQualifiedTypeReference(AR, typeParams, 0, poss(source, 5));
//			valueDecl.type.sourceStart = pS;
//			valueDecl.type.sourceEnd = valueDecl.type.statementEnd = pE;
//			setGeneratedBy(valueDecl.type, source);
//			
//			MessageSend getter = new MessageSend();
//			setGeneratedBy(getter, source);
//			getter.sourceStart = pS;
//			getter.sourceEnd = getter.statementEnd = pE;
//			getter.selector = new char[] {'g', 'e', 't'};
//			getter.receiver = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source);
//			
//			valueDecl.initialization = getter;
//			setGeneratedBy(valueDecl.initialization, source);
//			statements[0] = valueDecl;
//		}
//		
//		/*
//		 * if (value == null) { synchronized (this.fieldName) { value =
//		 * this.fieldName.get(); if (value == null) { value = new
//		 * java.util.concurrent.atomic.AtomicReference<ValueType>(new
//		 * ValueType()); this.fieldName.set(value); } } }
//		 */{
//			EqualExpression cond = new EqualExpression(new SingleNameReference(valueName, p), new NullLiteral(pS, pE), BinaryExpression.EQUAL_EQUAL);
//			setGeneratedBy(cond.left, source);
//			setGeneratedBy(cond.right, source);
//			setGeneratedBy(cond, source);
//			Block then = new Block(0);
//			setGeneratedBy(then, source);
//			Expression lock = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source);
//			Block inner = new Block(0);
//			setGeneratedBy(inner, source);
//			inner.statements = new Statement[2];
//			/* value = this.fieldName.get(); */{
//				MessageSend getter = new MessageSend();
//				setGeneratedBy(getter, source);
//				getter.sourceStart = pS;
//				getter.sourceEnd = getter.statementEnd = pE;
//				getter.selector = new char[] {'g', 'e', 't'};
//				getter.receiver = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source);
//				Assignment assign = new Assignment(new SingleNameReference(valueName, p), getter, pE);
//				assign.statementEnd = assign.sourceEnd;
//				setGeneratedBy(assign, source);
//				setGeneratedBy(assign.lhs, source);
//				inner.statements[0] = assign;
//			}
//			/* if (value == null) */{
//				EqualExpression innerCond = new EqualExpression(new SingleNameReference(valueName, p), new NullLiteral(pS, pE), BinaryExpression.EQUAL_EQUAL);
//				setGeneratedBy(innerCond.left, source);
//				setGeneratedBy(innerCond.right, source);
//				setGeneratedBy(innerCond, source);
//				Block innerThen = new Block(0);
//				setGeneratedBy(innerThen, source);
//				innerThen.statements = new Statement[2];
//				/*
//				 * value = new
//				 * java.util.concurrent.atomic.AtomicReference<ValueType>(new
//				 * ValueType());
//				 */{
//					AllocationExpression create = new AllocationExpression();
//					setGeneratedBy(create, source);
//					create.sourceStart = pS;
//					create.sourceEnd = create.statementEnd = pE;
//					TypeReference[][] typeParams = AR_PARAMS.clone();
//					typeParams[4] = new TypeReference[] {copyType(componentType, source)};
//					create.type = new ParameterizedQualifiedTypeReference(AR, typeParams, 0, poss(source, 5));
//					create.type.sourceStart = pS;
//					create.type.sourceEnd = create.type.statementEnd = pE;
//					setGeneratedBy(create.type, source);
//					create.arguments = new Expression[] {field.initialization};
//					Assignment innerAssign = new Assignment(new SingleNameReference(valueName, p), create, pE);
//					innerAssign.statementEnd = innerAssign.sourceEnd;
//					
//					setGeneratedBy(innerAssign, source);
//					setGeneratedBy(innerAssign.lhs, source);
//					innerThen.statements[0] = innerAssign;
//				}
//				
//				/* this.fieldName.set(value); */{
//					MessageSend setter = new MessageSend();
//					setGeneratedBy(setter, source);
//					setter.sourceStart = pS;
//					setter.sourceEnd = setter.statementEnd = pE;
//					setter.receiver = createFieldAccessor(fieldNode, FieldAccess.ALWAYS_FIELD, source);
//					setter.selector = new char[] {'s', 'e', 't'};
//					setter.arguments = new Expression[] {new SingleNameReference(valueName, p)};
//					setGeneratedBy(setter.arguments[0], source);
//					innerThen.statements[1] = setter;
//				}
//				
//				IfStatement innerIf = new IfStatement(innerCond, innerThen, pS, pE);
//				setGeneratedBy(innerIf, source);
//				inner.statements[1] = innerIf;
//			}
//			
//			SynchronizedStatement sync = new SynchronizedStatement(lock, inner, pS, pE);
//			setGeneratedBy(sync, source);
//			then.statements = new Statement[] {sync};
//			
//			IfStatement ifStatement = new IfStatement(cond, then, pS, pE);
//			setGeneratedBy(ifStatement, source);
//			statements[1] = ifStatement;
//		}
//		
//		/* return value.get(); */{
//			MessageSend getter = new MessageSend();
//			setGeneratedBy(getter, source);
//			getter.sourceStart = pS;
//			getter.sourceEnd = getter.statementEnd = pE;
//			getter.selector = new char[] {'g', 'e', 't'};
//			getter.receiver = new SingleNameReference(valueName, p);
//			setGeneratedBy(getter.receiver, source);
//			
//			statements[2] = new ReturnStatement(getter, pS, pE);
//			setGeneratedBy(statements[2], source);
//		}
//		
//		// update the field type and init last
//		
//		/*
//		 * private final
//		 * java.util.concurrent.atomic.AtomicReference<java.util.concurrent
//		 * .atomic.AtomicReference<ValueType> fieldName = new
//		 * java.util.concurrent
//		 * .atomic.AtomicReference<java.util.concurrent.atomic
//		 * .AtomicReference<ValueType>>();
//		 */{
//			
//			LocalDeclaration first = (LocalDeclaration) statements[0];
//			TypeReference innerType = copyType(first.type, source);
//			
//			TypeReference[][] typeParams = AR_PARAMS.clone();
//			typeParams[4] = new TypeReference[] {copyType(innerType, source)};
//			TypeReference type = new ParameterizedQualifiedTypeReference(AR, typeParams, 0, poss(source, 5));
//			// Some magic here
//			type.sourceStart = -1;
//			type.sourceEnd = type.statementEnd = -2;
//			setGeneratedBy(type, source);
//			
//			field.type = type;
//			AllocationExpression init = new AllocationExpression();
//			// Some magic here
//			init.sourceStart = field.initialization.sourceStart;
//			init.sourceEnd = init.statementEnd = field.initialization.sourceEnd;
//			init.type = copyType(type, source);
//			field.initialization = init;
//		}
//		return statements;
//	}
}
