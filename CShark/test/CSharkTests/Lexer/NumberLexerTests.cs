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

	[Fact]
	public void ZeroLexer_ZeroEOF_IntToken()
	{
	    var zeroLexer = new ZeroLexer();
	    using (IReader reader = new Reader(new StringReader("0")))
	    {
		Assert.True(reader.MoveNext());
		Token t = zeroLexer.Scan(reader);
		Assert.NotNull(t);
		Assert.Equal(0, t.Text);
		Assert.Equal(TokenType.IntConstant, t.TokenType);
		Assert.Equal(1, t.Line);
		Assert.Equal(1, t.Column);
		Assert.False(reader.MoveNext());
	    }
	}

	[Fact]
	public void ZeroLexer_ZeroTrail_IntToken()
	{
	    var zeroLexer = new ZeroLexer();
	    using (IReader reader = new Reader(new StringReader("0_")))
	    {
		Assert.True(reader.MoveNext());
		Token t = zeroLexer.Scan(reader);
		Assert.NotNull(t);
		Assert.Equal(0, t.Text);
		Assert.Equal(TokenType.IntConstant, t.TokenType);
		Assert.Equal(1, t.Line);
		Assert.Equal(1, t.Column);
		Assert.True(reader.MoveNext());
		Assert.Equal('_', reader.Current);
	    }
	}

	[Fact]
	public void ZeroLexer_IntHexadecimals_Success()
	{
	    string[] hexes = new string[] { "0X123", "0XFFF", "0Xfff", "0Xaaa",
					    "0XAAA", "0xaaa", "0xAAA", "0XAaBbCc",
					    "0xaAbBcC", "0x1239", "0X1239"};
	    var zeroLexer = new ZeroLexer();
	    foreach (var hex in hexes)
	    {
		Console.WriteLine(hex);
		int expected = Convert.ToInt32(hex.Substring(2), 16);
		using (IReader reader = new Reader (new StringReader(hex+" ")))
		{
		    Assert.True(reader.MoveNext());
		    
		    Token t = zeroLexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.IntConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.False(reader.MoveNext());
		}
	    }
	}
    }
}
