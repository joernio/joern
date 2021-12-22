package io.joern.fuzzyc2cpg.parser.functions.builder;

import io.joern.fuzzyc2cpg.ModuleParser.Parameter_declContext;
import io.joern.fuzzyc2cpg.ModuleParser.Parameter_idContext;
import io.joern.fuzzyc2cpg.ast.AstNodeBuilder;
import io.joern.fuzzyc2cpg.ast.langc.functiondef.ParameterType;
import io.joern.fuzzyc2cpg.ast.functionDef.ParameterBase;
import io.joern.fuzzyc2cpg.ast.functionDef.ParameterList;
import io.joern.fuzzyc2cpg.parser.AstNodeFactory;
import io.joern.fuzzyc2cpg.parser.ParseTreeUtils;
import org.antlr.v4.runtime.ParserRuleContext;

public class ParameterListBuilder extends AstNodeBuilder<ParameterList> {

    @Override
    public void createNew(ParserRuleContext ctx) {
        item = new ParameterList();
        AstNodeFactory.initializeFromContext(item, ctx);
    }

    public void addParameter(Parameter_declContext aCtx) {
        ParameterBase param = AstNodeFactory.create(aCtx);

        String baseType = ParseTreeUtils.childTokenString(aCtx.param_decl_specifiers());
        String completeType = aCtx.parameter_id() != null ?
                determineCompleteType(aCtx.parameter_id(), baseType) :
                determineCompleteAnonymousType(aCtx, baseType);

        ((ParameterType) param.getType()).setBaseType(baseType);
        ((ParameterType) param.getType()).setCompleteType(completeType);

        item.addChild(param);
    }

    private String determineCompleteAnonymousType(Parameter_declContext ctx,
                                                  String baseType) {
        StringBuilder retType = new StringBuilder(baseType);

        if (ctx.parameter_ptrs() != null) {
            retType.append(" ");
            retType.append(ParseTreeUtils.childTokenString(ctx.parameter_ptrs()));
        }

        return retType.toString();
    }

    private String determineCompleteType(Parameter_idContext parameter_id,
                                         String baseType) {

        StringBuilder retType = new StringBuilder(baseType);

        // iterate until nesting level is reached
        // where type is given.
        while (parameter_id.parameter_name() == null) {
            final StringBuilder newCompleteType = new StringBuilder();

            newCompleteType.append("(");

            if (parameter_id.parameter_ptrs() != null) {
                newCompleteType.append(ParseTreeUtils.childTokenString(parameter_id.parameter_ptrs()));
                newCompleteType.append(" ");
            }
            if (parameter_id.type_suffix() != null) {
                newCompleteType.append(ParseTreeUtils.childTokenString(parameter_id.type_suffix()));
                newCompleteType.append(" ");
            }

            newCompleteType.append(retType);
            newCompleteType.append(")");
            retType = newCompleteType;
            parameter_id = parameter_id.parameter_id();
        }

        if (parameter_id.parameter_ptrs() != null) {
            retType.append(" ");
            retType.append(ParseTreeUtils.childTokenString(parameter_id.parameter_ptrs()));
        }

        if (parameter_id.type_suffix() != null) {
            retType.append(" ");
            retType.append(ParseTreeUtils.childTokenString(parameter_id.type_suffix()));
        }

        return retType.toString();
    }

}
