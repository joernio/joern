package io.joern.fuzzyc2cpg.parsetreetoast;

import static org.junit.Assert.assertEquals;

import io.joern.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import org.junit.Test;

public class PreprocessorTests {

    @Test
    public void NestedIfndefs() {
        String input = "#ifdef foo\n#else\n #ifdef foo\n#else\n#endif\n#endif";
        CompoundStatement item = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        assertEquals(0, item.getStatements().size());
    }

    @Test
    public void testPreElseSkipping() {
        String input = "#if foo\n bar(); #else\n foo(); foo(); #endif";
        CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
                .parseAndWalk(input);
        assertEquals(1, contentItem.getStatements().size());
    }

}
