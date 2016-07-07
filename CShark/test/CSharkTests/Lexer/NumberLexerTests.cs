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
			    double actual = NumberUtils.ReadMantissa(reader, pref);
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
			Assert.Throws<FormatException>(() => NumberUtils.ReadMantissa(reader, pref));
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
			    double actual = NumberUtils.ReadMantissa(reader, pref);
			    Assert.True(reader.MoveNext());
			    Assert.Equal(end, reader.Current);
			    Assert.Equal(expected, Convert.ToSingle(actual));
			}
		    }
		}
	    }
        }

	[Fact]
	public void ZeroEOF_ZeroLexer_IntToken()
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
	public void ZeroTrail_ZeroLexer_IntToken()
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
	public void IntHexadecimals_ZeroLexer_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (random.Next(2) == 0)? 'x' : 'X';
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
	public void IntHexadecimalsEOF_ZeroLexer_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (random.Next(2) == 0)? 'x' : 'X';
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
	public void UIntHexadecimals_ZeroLexer_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (random.Next(2) == 0)? 'x' : 'X';
                char uOrU = (random.Next(2) == 0)? 'u' : 'U';
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
	public void ShortHexadecimalLiteral_ZeroLexer_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (random.Next(2) == 0)? 'x' : 'X';
                char sOrS = (random.Next(2) == 0)? 's' : 'S';
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
	public void UShortHexadecimals_x_ZeroLexer_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char sOrS = (random.Next(2) == 0)? 's' : 'S';
                char uOrU = (random.Next(2) == 0)? 'u' : 'U';
                ushort expected = Convert.ToUInt16(random.Next(15000));
                string hex  = string.Format("0x{0}{1}{2}", expected.ToString("x"), sOrS, uOrU);
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
	public void ShortUHexadecimalsLiteral_0X_ZeroLexer_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char sOrS = (random.Next(2) == 0)? 's' : 'S';
                char uOrU = (random.Next(2) == 0)? 'u' : 'U';
                ushort expected = Convert.ToUInt16(random.Next(15000));
                string hex  = string.Format("0X{0}{1}{2}", expected.ToString("x"),uOrU, sOrS);
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
	public void LongHexaDecimal_ZeroLexer_Success()
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
	public void ULongHexaDecimalLiteral_0x_ZeroLexer_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char xOrX = (i%2 == 0)? 'x' : 'X';
                char lOrL = (i%3 == 0)? 'l' : 'L';
                char uOrU = (i%4 == 0)? 'u' : 'U';
                ulong expected = Convert.ToUInt64(random.Next());
                string hex  = string.Format("0x{1}{2}{3}", xOrX, expected.ToString("x"), lOrL, uOrU);
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
	public void ULongHexaDecimalLiteral_0X_ZeroLexer_Success()
	{
            Random random = new Random();
            ILexer zeroLexer = new ZeroLexer();
            for (int i=0; i < 100; i++)
            {
                char lOrL = (i%3 == 0)? 'l' : 'L';
                char uOrU = (i%4 == 0)? 'u' : 'U';
                ulong expected = Convert.ToUInt64(random.Next());
                string hex  = string.Format("0X{0}{1}{2}", expected.ToString("x"), uOrU, lOrL);
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
	public void DoubleLiteral_NumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new NumberLexer();
            string followUps = "abcghijkmnopqrtvwxyz; \t!@#$%^&*(),<>/?\\\n";
            for (int i=0; i < 100; i++)
            {
                double expected = Convert.ToDouble(random.Next()) + random.NextDouble();
                char nextChar = followUps[random.Next(followUps.Length)];
		using (IReader reader = new Reader (new StringReader(expected.ToString() + nextChar)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected.ToString(), t.Text.ToString());
		    Assert.Equal(TokenType.DoubleConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

	[Fact]
	public void FloatLiteral_NumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new NumberLexer();
            string followUps = "abcghijkmnopqrtvwxyz; \t!@#$%^&*(),<>/?\\\n";
            for (int i=0; i < 100; i++)
            {
                float expected = Convert.ToSingle(random.Next()) + Convert.ToSingle(random.NextDouble());
                char nextChar = followUps[random.Next(followUps.Length)];
                char suff = (i%2 == 0) ? 'f' : 'F';
                string inpStr = $"{expected}{suff}{nextChar}";
                using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected.ToString(), t.Text.ToString());
		    Assert.Equal(TokenType.FloatConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

	[Fact]
	public void IntLiteral_NumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new NumberLexer();
            string followUps = "abceghijkmnopqrtvwxz; \t!@#$%^&*(),<>/?\\\n";
            for (int i=0; i < 100; i++)
            {
                int expected = random.Next();
                char nextChar = followUps[random.Next(followUps.Length)];
		using (IReader reader = new Reader (new StringReader(expected.ToString() + nextChar)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.IntConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void IntLiteralEOF_NumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new NumberLexer();
            for (int i=0; i < 100; i++)
            {
                int expected = random.Next();
		using (IReader reader = new Reader (new StringReader(expected.ToString())))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
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
	public void UIntLiteral_NumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new NumberLexer();
            string followUps = "abceghijkmnopqrtvwxz;.; \t!@#$%^&*(),.<>/?\\\n";
            for (int i=0; i < 100; i++)
            {
                uint expected = Convert.ToUInt32(random.Next());
                char uOrU = (i % 2 == 0)? 'u' : 'U';
                char nextChar = followUps[random.Next(followUps.Length)];
                var inpStr = $"{expected}{uOrU}{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.UIntConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
		}
	    }
        }

        [Fact]
	public void ShortIntLiteral_NumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new NumberLexer();
            string followUps = "abceghijkmnopqrtvwxyz; \t!@#$%^&*(),<>/?\\\n";
            for (int i=0; i < 100; i++)
            {
                short expected = Convert.ToInt16(random.Next(15000));
                char nextChar = followUps[random.Next(followUps.Length)];
                char sOrS = (i % 2 == 0)? 's' : 'S';
                var inpStr = $"{expected}{sOrS}{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.ShortConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void LongLiteral_NumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new NumberLexer();
            string followUps = "abceghijkmnopqrtvwxyz; \t!@#$%^&*(),<>/?\\\n";
            for (int i=0; i < 100; i++)
            {
                long expected = Convert.ToInt64(random.Next()) * Convert.ToInt64(random.Next());
                char nextChar = followUps[random.Next(followUps.Length)];
                char lOrL = (i % 2 == 0)? 'l' : 'L';
                var inpStr = $"{expected}{lOrL}{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.LongConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void UShortLiteral_NumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new NumberLexer();
            string followUps = "abceghijkmnopqrtvwxyz; \t!@#$%^&*(),<>/?\\\n";
            string[] suffixes = new string[] { "SU", "su", "Su", "sU" };
            for (int i=0; i < 100; i++)
            {
                ushort expected = Convert.ToUInt16(random.Next(15000));
                char nextChar = followUps[random.Next(followUps.Length)];
                string suffix = suffixes[random.Next(4)];
                var inpStr = $"{expected}{suffix}{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.UShortConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void ULongLiteral_NumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new NumberLexer();
            string followUps = "abceghijkmnopqrtvwxyz; \t!@#$%^&*(),<>/?\\\n";
            string[] suffixes = new string[] { "LU", "lu", "Lu", "lU" };
            for (int i=0; i < 100; i++)
            {
                ulong expected = Convert.ToUInt64(random.Next()) * Convert.ToUInt64(random.Next());
                char nextChar = followUps[random.Next(followUps.Length)];

                string suffix = suffixes[random.Next(4)];
                var inpStr = $"{expected}{suffix}{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.ULongConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void ShortLiteralWithSignPrefix_SignedNumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new SignedNumberLexer();
            string followUps = "abceghijkmnopqrtvwxyz; \t!@#$%^&*(),<>/?\\\n";
            string[] prefixes = new string[] { "-", "+"};
            for (int i=0; i < 100; i++)
            {
                short expected = Convert.ToInt16(random.Next(1500));
                char nextChar = followUps[random.Next(followUps.Length)];
                string prefix = prefixes[random.Next(2)];
                var inpStr = $"{prefix}{expected}S{nextChar}";

		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    int prod = (prefix == "-") ? -1 : 1;
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(Convert.ToInt16(expected * prod), t.Text);
		    Assert.Equal(TokenType.ShortConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void IntLiteralWithSignPrefix_SignedNumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new SignedNumberLexer();
            string followUps = "abceghijkmnopqrtvwxz; \t!@#$%^&*(),<>/?\\\n";
            string[] prefixes = new string[] { "-", "+"};
            for (int i=0; i < 100; i++)
            {
                int expected = random.Next();
                char nextChar = followUps[random.Next(followUps.Length)];
                string prefix = prefixes[random.Next(2)];
                var inpStr = $"{prefix}{expected}{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    int prod = (prefix == "-") ? -1 : 1;
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected * prod, t.Text);
		    Assert.Equal(TokenType.IntConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void LongConstWithSignPrefix_SignedNumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new SignedNumberLexer();
            string followUps = "abceghijkmnopqrtvwxyz; \t!@#$%^&*(),<>/?\\\n";
            string[] prefixes = new string[] { "-", "+"};
            for (int i=0; i < 100; i++)
            {
                long expected = Convert.ToInt64(random.Next());
                char nextChar = followUps[random.Next(followUps.Length)];
                string prefix = prefixes[random.Next(2)];
                var inpStr = $"{prefix}{expected}L{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    int prod = (prefix == "-") ? -1 : 1;
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected * prod, t.Text);
		    Assert.Equal(TokenType.LongConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void FloatLiteralWithSignPrefix_SignedNumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new SignedNumberLexer();
            string followUps = "abceghijkmnopqrtvwxyz; \t!@#$%^&*(),<>/?\\\n";
            string[] prefixes = new string[] { "-", "+"};
            for (int i=0; i < 100; i++)
            {
                float expected = Convert.ToSingle(random.Next(999999)) + Convert.ToSingle(random.NextDouble());
                char nextChar = followUps[random.Next(followUps.Length)];
                string prefix = prefixes[random.Next(2)];
                var inpStr = $"{prefix}{expected}f{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    float expVal = expected * ((prefix == "-") ? -1 : 1);
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expVal.ToString(), t.Text.ToString());
		    Assert.Equal(TokenType.FloatConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void DoubleConstWithSignPrefix_SignedNumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new SignedNumberLexer();
            string followUps = "abcghijkmnopqrtvwxyz; \t!@#$%^&*(),<>/?\\\n";
            string[] prefixes = new string[] { "-", "+"};
            for (int i=0; i < 100; i++)
            {
                double expected = Convert.ToDouble(random.Next()) + random.NextDouble();
                char nextChar = followUps[random.Next(followUps.Length)];
                string prefix = prefixes[random.Next(2)];
                var inpStr = $"{prefix}{expected}{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    double expVal = expected * ((prefix == "-") ? -1d : 1d);
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		   // not working somehow - Assert.Equal(expVal, (double)(t.Text), 2);
		    Assert.Equal(expVal.ToString(), t.Text.ToString());
		    Assert.Equal(TokenType.DoubleConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void SignAndNoNumber_SignLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new SignedNumberLexer();
            string followUps = "abcghijkmnopqrtvwxyz; \t!@#$%^&*(),<>/?\\\n";
            string[] prefixes = new string[] { "-", "+"};
            for (int i=0; i < 100; i++)
            {
                char nextChar = followUps[random.Next(followUps.Length)];
                string prefix = prefixes[random.Next(2)];
                var inpStr = $"{prefix}{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(prefix, t.Text);
		    Assert.Equal(TokenType.Identifier, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void ByteLiteralWithSignPrefix_SignedNumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new SignedNumberLexer();
            string followUps = "abcdefghijklmnopqrstvwxyz; \t!@#$%^&*(),<>/?\\\n";
            string[] prefixes = new string[] { "-", "+"};
            for (int i=0; i < 100; i++)
            {
                sbyte expected = Convert.ToSByte(random.Next(128));
                char nextChar = followUps[random.Next(followUps.Length)];
                string prefix = prefixes[random.Next(2)];
                var inpStr = $"{prefix}{expected}y{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    int prod = (prefix == "-") ? -1 : 1;
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(Convert.ToSByte(expected * prod), t.Text);
		    Assert.Equal(TokenType.SByteConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void UnsignedByteLiteral_NumberLexer_Success()
	{
            Random random = new Random();
            ILexer lexer = new NumberLexer();
            string followUps = "abcdefghijklmnopqrstuvwxyz; \t!@#$%^&*(),<>/?\\\n";
            string[] suffixes = new string[] { "YU", "yu", "Yu", "yU" };
            for (int i=0; i < 100; i++)
            {
                byte expected = Convert.ToByte(random.Next(256));
                char nextChar = followUps[random.Next(followUps.Length)];

                string suffix = suffixes[random.Next(4)];
                var inpStr = $"{expected}{suffix}{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.ByteConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}

        [Fact]
	public void SByteLiteral_NumberLexer_Success()
	{
	    Random random =new Random();
            ILexer lexer = new NumberLexer();
            string followUps = "abcdefghijklmnopqrstvwxyz; \t!@#$%^&*(),<>/?\\\n";
            for (int i=0; i < 128; i++)
            {
                sbyte expected = Convert.ToSByte(i);
                char nextChar = followUps[random.Next(followUps.Length)];
		char suffix = (random.Next(2) == 0) ? 'y' : 'Y';

                var inpStr = $"{expected}{suffix}{nextChar}";
		using (IReader reader = new Reader (new StringReader(inpStr)))
		{
		    Assert.True(reader.MoveNext());
		    Token t = lexer.Scan(reader);
		    Assert.NotNull(t);
		    Assert.Equal(expected, t.Text);
		    Assert.Equal(TokenType.SByteConstant, t.TokenType);
		    Assert.Equal(1, t.Line);
		    Assert.Equal(1, t.Column);
		    Assert.True(reader.MoveNext());
                    Assert.Equal(nextChar, reader.Current);
		}
	    }
	}
    }
}
