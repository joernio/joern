package io.shiftleft.fuzzyc2cpg.parsetreetoast;

import static org.junit.Assert.assertTrue;

import io.shiftleft.fuzzyc2cpg.ast.logical.statements.CompoundStatement;
import org.junit.Test;

public class PreprocessorTests
{

	@Test
	public void NestedIfndefs()
	{
		String input = "#ifdef foo\n#else\n #ifdef foo\n#else\n#endif\n#endif";
		CompoundStatement item = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		assertTrue(item.getStatements().size() == 0);
	}

	@Test
	public void testPreElseSkipping()
	{
		String input = "#if foo\n bar(); #else\n foo(); foo(); #endif";
		CompoundStatement contentItem = (CompoundStatement) FunctionContentTestUtil
				.parseAndWalk(input);
		assertTrue(contentItem.getStatements().size() == 1);
	}

}
