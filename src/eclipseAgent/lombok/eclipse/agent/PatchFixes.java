/*
 * Copyright (C) 2010 The Project Lombok Authors.
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

package lombok.eclipse.agent;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import lombok.core.DiagnosticsReceiver;
import lombok.core.PostCompiler;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IAnnotatable;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.dom.SimpleName;
import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.Annotation;
import org.eclipse.jdt.internal.core.dom.rewrite.NodeRewriteEvent;
import org.eclipse.jdt.internal.core.dom.rewrite.RewriteEvent;
import org.eclipse.jdt.internal.core.dom.rewrite.TokenScanner;

public class PatchFixes {
	public static boolean isGenerated(org.eclipse.jdt.core.dom.Statement statement) {
		boolean result = false;
		try {
			result =  ((Boolean)statement.getClass().getField("$isGenerated").get(statement)).booleanValue();
		} catch (Exception e) {
			// better to assume it isn't generated
		}
		return result;
	}

	public static org.eclipse.jdt.core.ISourceRange fixJavaEditorSetSelection(org.eclipse.jdt.core.ISourceRange original, org.eclipse.jdt.core.ISourceReference reference) throws org.eclipse.jdt.core.JavaModelException {
		if (reference instanceof org.eclipse.jdt.internal.core.SourceRefElement) {
			Object elementInfo = ((org.eclipse.jdt.internal.core.SourceRefElement) reference).getElementInfo();
			if (elementInfo instanceof org.eclipse.jdt.internal.core.AnnotatableInfo) {
				try {
					org.eclipse.jdt.core.SourceRange range = (org.eclipse.jdt.core.SourceRange)elementInfo.getClass().getField("$jumpToRange").get(elementInfo);
					if (range != null)
						return range;
				} catch (Exception e) {
					// return original
				}
			}
		}
		return original;
	}

	public static org.eclipse.jdt.internal.core.SourceMethodElementInfo fixSetJumpToLocationMethod(org.eclipse.jdt.internal.core.SourceMethodElementInfo info, org.eclipse.jdt.internal.compiler.ISourceElementRequestor.MethodInfo internalInfo) throws Exception {
		Object generatedBy = internalInfo.node.getClass().getField("$generatedBy").get(internalInfo.node);
		if (generatedBy != null) {
			ASTNode node = (org.eclipse.jdt.internal.compiler.ast.ASTNode)generatedBy;
			info.getClass().getField("$jumpToRange").set(info, new org.eclipse.jdt.core.SourceRange(node.sourceStart, node.sourceEnd-node.sourceStart+1));
		}
		return info;
	}

	public static org.eclipse.jdt.internal.core.SourceTypeElementInfo fixSetJumpToLocationType(org.eclipse.jdt.internal.core.SourceTypeElementInfo info, org.eclipse.jdt.internal.compiler.ISourceElementRequestor.TypeInfo internalInfo) throws Exception {
		Object generatedBy = internalInfo.node.getClass().getField("$generatedBy").get(internalInfo.node);
		if (generatedBy != null) {
			ASTNode node = (org.eclipse.jdt.internal.compiler.ast.ASTNode)generatedBy;
			info.getClass().getField("$jumpToRange").set(info, new org.eclipse.jdt.core.SourceRange(node.sourceStart, node.sourceEnd-node.sourceStart+1));
		}
		return info;
	}

	public static org.eclipse.jdt.internal.core.SourceFieldElementInfo fixSetJumpToLocationField(org.eclipse.jdt.internal.core.SourceFieldElementInfo info, org.eclipse.jdt.internal.core.CompilationUnitStructureRequestor requestor) throws Exception {
		Stack<?> stack = (Stack<?>)requestor.getClass().getDeclaredField("infoStack").get(requestor);
		org.eclipse.jdt.internal.compiler.ISourceElementRequestor.FieldInfo internalInfo = (org.eclipse.jdt.internal.compiler.ISourceElementRequestor.FieldInfo)stack.peek();
		Object generatedBy = internalInfo.node.getClass().getField("$generatedBy").get(internalInfo.node);
		if (generatedBy != null) {
			ASTNode node = (org.eclipse.jdt.internal.compiler.ast.ASTNode)generatedBy;
			info.getClass().getField("$jumpToRange").set(info, new org.eclipse.jdt.core.SourceRange(node.sourceStart, node.sourceEnd-node.sourceStart+1));
		}
		return info;
	}
	
	public static int fixRetrieveStartingCatchPosition(int original, int start) {
		return original == -1 ? start : original;
	}
	
	public static int fixRetrieveIdentifierEndPosition(int original, int end) {
		return original == -1 ? end : original;
	}
	
	public static int fixRetrieveEllipsisStartPosition(int original, int end) {
		return original == -1 ? end : original;
	}

	public static int fixRetrieveRightBraceOrSemiColonPosition(int original, int end) {
		return original == -1 ? end : original;
	}
	
	public static final int ALREADY_PROCESSED_FLAG = 0x800000;	//Bit 24
	
	public static boolean checkBit24(Object node) throws Exception {
		int bits = (Integer)(node.getClass().getField("bits").get(node));
		return (bits & ALREADY_PROCESSED_FLAG) != 0;
	}
	
	public static boolean skipRewritingGeneratedNodes(org.eclipse.jdt.core.dom.ASTNode node) throws Exception {
		return ((Boolean)node.getClass().getField("$isGenerated").get(node)).booleanValue();
	}
	
	public static void setIsGeneratedFlag(org.eclipse.jdt.core.dom.ASTNode domNode,
			org.eclipse.jdt.internal.compiler.ast.ASTNode internalNode) throws Exception {
		if (internalNode == null || domNode == null) return;
		boolean isGenerated = internalNode.getClass().getField("$generatedBy").get(internalNode) != null;
		if (isGenerated) {
			domNode.getClass().getField("$isGenerated").set(domNode, true);
			domNode.setFlags(domNode.getFlags() & ~org.eclipse.jdt.core.dom.ASTNode.ORIGINAL);
		}
	}
	
	public static void setIsGeneratedFlagForSimpleName(SimpleName name, Object internalNode) throws Exception {
		if (internalNode instanceof org.eclipse.jdt.internal.compiler.ast.ASTNode) {
			if (internalNode.getClass().getField("$generatedBy").get(internalNode) != null) {
				name.getClass().getField("$isGenerated").set(name, true);
			}
		}
	}
	
	public static RewriteEvent[] listRewriteHandleGeneratedMethods(RewriteEvent parent) {
		RewriteEvent[] children = parent.getChildren();
		List<RewriteEvent> newChildren = new ArrayList<RewriteEvent>();
		List<RewriteEvent> modifiedChildren = new ArrayList<RewriteEvent>();
		for (int i=0; i<children.length; i++) {
			RewriteEvent child = children[i];
			boolean isGenerated = false;
			try {
				org.eclipse.jdt.core.dom.ASTNode originalValue = (org.eclipse.jdt.core.dom.ASTNode)child.getOriginalValue();
				isGenerated = (Boolean) originalValue.getClass().getField("$isGenerated").get(originalValue);
			} catch (Exception e) {
				// If this fails, better to break some refactor scripts than to crash eclipse.
			}
			if (isGenerated
				&& (child.getChangeKind() == RewriteEvent.REPLACED || child.getChangeKind() == RewriteEvent.REMOVED) 
				&& child.getOriginalValue() instanceof org.eclipse.jdt.core.dom.MethodDeclaration
			) {
				if (child.getNewValue() != null)
					modifiedChildren.add(new NodeRewriteEvent(null, child.getNewValue()));
			} else {
				newChildren.add(child);
			}
		}
		// Since Eclipse doesn't honor the "insert at specified location" for already existing members,
		// we'll just add them last
		newChildren.addAll(modifiedChildren);
		return newChildren.toArray(new RewriteEvent[newChildren.size()]);
	}

	public static int getTokenEndOffsetFixed(TokenScanner scanner, int token, int startOffset, Object domNode) throws CoreException {
		boolean isGenerated = false;
		try {
			isGenerated = (Boolean) domNode.getClass().getField("$isGenerated").get(domNode);
		} catch (Exception e) {
			// If this fails, better to break some refactor scripts than to crash eclipse.
		}
		if (isGenerated) return -1;
		return scanner.getTokenEndOffset(token, startOffset);
	}
	
	public static IMethod[] removeGeneratedMethods(IMethod[] methods) throws Exception {
		List<IMethod> result = new ArrayList<IMethod>();
		for (IMethod m : methods) {
			if (m.getNameRange().getLength() > 0) result.add(m);
		}
		return result.size() == methods.length ? methods : result.toArray(new IMethod[0]);
	}
	
	public static SimpleName[] removeGeneratedSimpleNames(SimpleName[] in) throws Exception {
		Field f = SimpleName.class.getField("$isGenerated");
		
		int count = 0;
		for (int i = 0; i < in.length; i++) {
			if (in[i] == null || !((Boolean)f.get(in[i])).booleanValue()) count++;
		}
		if (count == in.length) return in;
		SimpleName[] newSimpleNames = new SimpleName[count];
		count = 0;
		for (int i = 0; i < in.length; i++) {
			if (in[i] == null || !((Boolean)f.get(in[i])).booleanValue()) newSimpleNames[count++] = in[i];
		}
		return newSimpleNames;
	}
	
	public static byte[] runPostCompiler(byte[] bytes, String fileName) {
		byte[] transformed = PostCompiler.applyTransformations(bytes, fileName, DiagnosticsReceiver.CONSOLE);
		return transformed == null ? bytes : transformed;
	}
	
	public static OutputStream runPostCompiler(OutputStream out) throws IOException {
		return PostCompiler.wrapOutputStream(out, "TEST", DiagnosticsReceiver.CONSOLE);
	}
	
	public static BufferedOutputStream runPostCompiler(BufferedOutputStream out, String path, String name) throws IOException {
		String fileName = path + "/" + name;
		return new BufferedOutputStream(PostCompiler.wrapOutputStream(out, fileName, DiagnosticsReceiver.CONSOLE));
	}
	
	public static Annotation[] convertAnnotations(Annotation[] out, IAnnotatable annotatable) {
		IAnnotation[] in;
		
		try {
			in = annotatable.getAnnotations();
		} catch (Exception e) {
			return out;
		}
		
		if (out == null) return null;
		int toWrite = 0;
		
		for (int idx = 0; idx < out.length; idx++) {
			String oName = new String(out[idx].type.getLastToken());
			boolean found = false;
			for (IAnnotation i : in) {
				String name = i.getElementName();
				int li = name.lastIndexOf('.');
				if (li > -1) name = name.substring(li + 1);
				if (name.equals(oName)) {
					found = true;
					break;
				}
			}
			if (!found) out[idx] = null;
			else toWrite++;
		}
		
		Annotation[] replace = out;
		if (toWrite < out.length) {
			replace = new Annotation[toWrite];
			int idx = 0;
			for (int i = 0; i < out.length; i++) {
				if (out[i] == null) continue;
				replace[idx++] = out[i];
			}
		}
		
		return replace;
	}
}