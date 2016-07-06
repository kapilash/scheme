// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.IO;
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
    }
}
