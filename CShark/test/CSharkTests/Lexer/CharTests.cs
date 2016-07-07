// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.IO;
using System.Text;
using Xunit;

using CShark.Lexer;
namespace CSharkTests.Lexer
{
    public class CharTests
    {
        [Fact]
        public void RandomlyChosenChars_As4DigitHexStr_CharLexer_Match()
        {
            Random random = new Random();
            ILexer lexer = new CharLexer();
            for (int i=0; i<1000; i++)
            {
                char c = Convert.ToChar(random.Next(Char.MaxValue));
                string inpStr = Convert.ToInt32(c).ToString("x");
                while (inpStr.Length < 4)
                {
                    inpStr = "0" + inpStr;
                }
                Assert.Equal(4, inpStr.Length);

                using (IReader reader = new Reader(new StringReader($"'\\u{inpStr}'")))
                {
                    Assert.True(reader.MoveNext());
                    var token = lexer.Scan(reader);
                    Assert.Equal(TokenType.CharConstant, token.TokenType);
                    Assert.Equal(c, token.Text);
                    Assert.False(reader.MoveNext());
                }
            }
        }

        [Fact]
        public void RandomlyChosenChars_As8DigitHexStr_CharLexer_Match()
        {
            Random random = new Random();
            ILexer lexer = new CharLexer();
            for (int i=0; i<1000; i++)
            {
                char c = Convert.ToChar(random.Next(Char.MaxValue));
                string inpStr = Convert.ToInt32(c).ToString("x");
                while (inpStr.Length < 8)
                {
                    inpStr = "0" + inpStr;
                }
                Assert.Equal(8, inpStr.Length);

                using (IReader reader = new Reader(new StringReader($"'\\U{inpStr}'")))
                {
                    Assert.True(reader.MoveNext());
                    var token = lexer.Scan(reader);
                    Assert.Equal(TokenType.CharConstant, token.TokenType);
                    Assert.Equal(c, token.Text);
                    Assert.False(reader.MoveNext());
                }
            }
        }

        [Fact]
        public void EscapedChars_CharLexer_Match()
        {
            ILexer lexer = new CharLexer();
            for(int i=0; i<CharLexer.EscapedChars.Length; i++)
            {
                char c = CharLexer.EscapedChars[i];
                var expected = CharLexer.EscapedVals[i];
                using (IReader reader = new Reader(new StringReader($"'\\{c}'")))
                {
                    Assert.True(reader.MoveNext());
                    var token = lexer.Scan(reader);
                    Assert.Equal(TokenType.CharConstant, token.TokenType);
                    Assert.Equal(expected, token.Text);
                    Assert.False(reader.MoveNext());
                }                
            }
        }

        [Fact]
        public void RandomlyChosenChars_CharLexer_Match()
        {
            Random random = new Random();
            ILexer lexer = new CharLexer();
            for (int i=0; i<1000; i++)
            {
                char c = Convert.ToChar(random.Next(Char.MaxValue));
                while (c == '\\' || c == '\'')
                {
                    c = Convert.ToChar(random.Next(Char.MaxValue));
                }

                using (IReader reader = new Reader(new StringReader($"'{c}'")))
                {
                    Assert.True(reader.MoveNext());
                    var token = lexer.Scan(reader);
                    Assert.Equal(TokenType.CharConstant, token.TokenType);
                    Assert.Equal(c, token.Text);
                    Assert.False(reader.MoveNext());
                }
            }
        }

        [Fact]
        public void EmptyChar_CharLexer_Throws()
        {
            var lexer = new CharLexer();
            using (IReader reader = new Reader(new StringReader($"''")))
            {
                Assert.True(reader.MoveNext());
                Assert.Throws<ScannerException>(() => lexer.Scan(reader));
            }
        }

        [Fact]
        public void UnescapedBackSlash_CharLexer_Throws()
        {
            var lexer = new CharLexer();
            using (IReader reader = new Reader(new StringReader($"'\\'")))
            {
                Assert.True(reader.MoveNext());
                Assert.Throws<ScannerException>(() => lexer.Scan(reader));
            }
        }

        [Theory]
        [InlineData(0)]
        [InlineData(1)]
        [InlineData(2)]
        [InlineData(3)]
        [InlineData(5)]
        public void WrongNumOfHexDigitsFollowing_u_CharLexer_Throws(int count)
        {
            string digits = "0123456789ABCDEFabcdef";
            var random = new Random();
            var lexer = new CharLexer();
            for (int i=0; i<1000; i++)
            {
                var strb = new StringBuilder("'\\u");
                for (int j=0; j < count; j++)
                {
                    strb.Append(digits[random.Next(digits.Length)]);
                }

                using (IReader reader = new Reader(new StringReader(strb.ToString())))
                {
                    Assert.True(reader.MoveNext());
                    Assert.Throws<ScannerException>(() => lexer.Scan(reader));
                }
            }
        }

        [Theory]
        [InlineData(0)]
        [InlineData(1)]
        [InlineData(2)]
        [InlineData(3)]
        [InlineData(4)]
        [InlineData(5)]
        [InlineData(6)]
        [InlineData(7)]
        [InlineData(9)]
        public void WrongNumOfHexDigitsFollowing_U_CharLexer_Throws(int count)
        {
            string digits = "0123456789ABCDEFabcdef";
            var random = new Random();
            var lexer = new CharLexer();
            for (int i=0; i<10; i++)
            {
                var strb = new StringBuilder("'\\U");
                for (int j=0; j < count; j++)
                {
                    strb.Append(digits[random.Next(digits.Length)]);
                }

                using (IReader reader = new Reader(new StringReader(strb.ToString())))
                {
                    Assert.True(reader.MoveNext());
                    Assert.Throws<ScannerException>(() => lexer.Scan(reader));
                }
            }
        }
    }
}
