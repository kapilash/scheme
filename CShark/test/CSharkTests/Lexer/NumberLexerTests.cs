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
        public readonly static object[][] UIntHexaDecimals = {
            new object[] { "0X123u", 0X123u },
            new object[] { "0XFFFu", 0XFFFu },
            new object[] { "0XfffU", 0XfffU },
            new object[] { "0XaaaU", 0XaaaU },
            new object[] { "OXAAAu", 0XAAAu },
            new object[] { "0xaaau", 0xaaau },
            new object[] { "0XAaBbCcU", 0XAaBbCcU },
            new object[] { "0xaAbBcCu", 0xaAbBcCu },
            new object[] { "0x1239U", 0x1239U },
            new object[] { "0X1239u", 0X1239u }
        };

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
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (i%2 == 0)? 'x' : 'X';
                int expected = random.Next();
                string hex  = string.Format("0{0}{1} ", xOrX, expected.ToString("x"));
		using (IReader reader = new Reader (new StringReader(hex)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = zeroLexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.IntConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(' ', reader.Current);
		}
	    }
	}

        [Fact]
	public void ZeroLexer_IntHexadecimalsEOF_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (i%2 == 0)? 'x' : 'X';
                int expected = random.Next();
                string hex  = string.Format("0{0}{1}", xOrX, expected.ToString("x"));
		using (IReader reader = new Reader (new StringReader(hex)))
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

	[Fact]
	public void ZeroLexer_UIntHexadecimals_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (i%2 == 0)? 'x' : 'X';
                char uOrU = (i%3 == 0)? 'u' : 'U';
                uint expected = Convert.ToUInt32(random.Next());
                string hex  = string.Format("0{0}{1}{2}", xOrX, expected.ToString("x"), uOrU);
		using (IReader reader = new Reader (new StringReader(hex)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = zeroLexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.UIntConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.False(reader.MoveNext());
		}
	    }
	}

	[Fact]
	public void ZeroLexer_ShortHexadecimals_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (i%2 == 0)? 'x' : 'X';
                char sOrS = (i%3 == 0)? 's' : 'S';
                short expected = Convert.ToInt16(random.Next(15000));
                string hex  = string.Format("0{0}{1}{2}", xOrX, expected.ToString("x"), sOrS);
		using (IReader reader = new Reader (new StringReader(hex)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = zeroLexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.ShortConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.False(reader.MoveNext());
		}
	    }
        }

	[Fact]
	public void ZeroLexer_UShortHexadecimals_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (i%2 == 0)? 'x' : 'X';
                char sOrS = (i%3 == 0)? 's' : 'S';
                char uOrU = (i%4 == 0)? 'u' : 'U';
                ushort expected = Convert.ToUInt16(random.Next(15000));
                string hex  = string.Format("0{0}{1}{2}{3}", xOrX, expected.ToString("x"), sOrS, uOrU);
		using (IReader reader = new Reader (new StringReader(hex)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = zeroLexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.UShortConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.False(reader.MoveNext());
		}
	    }
	}

        [Fact]
	public void ZeroLexer_ShortUHexadecimals_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (i%2 == 0)? 'x' : 'X';
                char sOrS = (i%3 == 0)? 's' : 'S';
                char uOrU = (i%4 == 0)? 'u' : 'U';
                ushort expected = Convert.ToUInt16(random.Next(15000));
                string hex  = string.Format("0{0}{1}{2}{3}", xOrX, expected.ToString("x"),uOrU, sOrS);
		using (IReader reader = new Reader (new StringReader(hex)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = zeroLexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.UShortConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.False(reader.MoveNext());
		}
	    }
	}

	[Fact]
	public void ZeroLexer_LongHexadecimals_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (i%2 == 0)? 'x' : 'X';
                char lOrL = (i%3 == 0)? 'l' : 'L';
                long expected = Convert.ToInt64(random.Next());
                string hex  = string.Format("0{0}{1}{2}", xOrX, expected.ToString("x"), lOrL);
		using (IReader reader = new Reader (new StringReader(hex)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = zeroLexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.LongConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.False(reader.MoveNext());
		}
	    }
	}

	[Fact]
	public void ZeroLexer_ULongHexadecimals_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (i%2 == 0)? 'x' : 'X';
                char lOrL = (i%3 == 0)? 'l' : 'L';
                char uOrU = (i%4 == 0)? 'u' : 'U';
                ulong expected = Convert.ToUInt64(random.Next());
                string hex  = string.Format("0{0}{1}{2}{3}", xOrX, expected.ToString("x"), lOrL, uOrU);
		using (IReader reader = new Reader (new StringReader(hex)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = zeroLexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.ULongConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.False(reader.MoveNext());
		}
	    }
	}

        [Fact]
	public void ZeroLexer_LongUHexadecimals_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (i%2 == 0)? 'x' : 'X';
                char lOrL = (i%3 == 0)? 'l' : 'L';
                char uOrU = (i%4 == 0)? 'u' : 'U';
                ulong expected = Convert.ToUInt64(random.Next());
                string hex  = string.Format("0{0}{1}{2}{3}", xOrX, expected.ToString("x"), uOrU, lOrL);
		using (IReader reader = new Reader (new StringReader(hex)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = zeroLexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.ULongConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.False(reader.MoveNext());
		}
	    }
	}
    }
}
