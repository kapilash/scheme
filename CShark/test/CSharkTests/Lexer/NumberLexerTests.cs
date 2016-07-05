// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.IO;
using Xunit;

using CShark.Lexer;
namespace CSharkTests.Lexer
{
    public class NumberLexerTests
    {
        [Fact]
        public void ValidMantissa_ReadMantissa_Double()
        {
            string[] prefixes = new string[] {"0", "19", "123", "4321", "98765", "712358" };
            string[] mants = new string[] { "0", "12", "123", "1234", "12345", "129E9", "129E-90", "129E+90",
                                            "129e90", "129e-910", "129e+90"};
	    char[] ends = new char[] { ' ', '_' , '.', 'A', 'Z', '=', 'a', 'z', '\t'};
	    foreach (var pref in prefixes)
	    {
		foreach (var mant in mants)
		{
		    double expectedDouble = Convert.ToDouble(pref + "." + mant);
		    foreach (var end in ends)
		    {
			using (IReader reader = new Reader(new StringReader(mant + end)))
			{
			    Assert.True(reader.MoveNext());
			    double actual = NumberLexer.ReadMantissa(reader, pref);
			    Assert.True(reader.MoveNext());
			    Assert.Equal(end, reader.Current);
			    Assert.Equal(expectedDouble, actual);
			}
		    }
		}
	    }
        }

        [Fact]
        public void InvalidMantissa_ReadMantissa_Throws()
        {
            string[] prefixes = new string[] {"0", "19", "123", "4321", "98765", "712358" };
            string[] mants = new string[] { "129E.9", "129EA90", "129e.90",
                                            "129ea90" };
	    foreach (var pref in prefixes)
	    {
		foreach (var mant in mants)
		{
		    using (IReader reader = new Reader(new StringReader(mant)))
		    {
			Assert.True(reader.MoveNext());
			Assert.Throws<FormatException>(() => NumberLexer.ReadMantissa(reader, pref));
		    }
		}
	    }
        }

        [Fact]
        public void ValidMantissa_ReadMantissa_Float()
        {
            string[] prefixes = new string[] {"0", "19", "123", "4321", "98765", "712358" };
            string[] mants = new string[] { "0", "12", "123", "1234", "12345", "129E2", "129E-2", "129E+3",
                                            "129e3", "129e-2", "129e+3"};
	    char[] ends = new char[] { ' ', '_' , '.', 'A', 'Z', '=', 'a', 'z', '\t'};
	    foreach (var pref in prefixes)
	    {
		foreach (var mant in mants)
		{
		    float expected = Convert.ToSingle(pref + "." + mant);
		    foreach (var end in ends)
		    {
			using (IReader reader = new Reader(new StringReader(mant + end)))
			{
			    Assert.True(reader.MoveNext());
			    double actual = NumberLexer.ReadMantissa(reader, pref);
			    Assert.True(reader.MoveNext());
			    Assert.Equal(end, reader.Current);
			    Assert.Equal(expected, Convert.ToSingle(actual));
			}
		    }
		}
	    }
        }
    }
}
