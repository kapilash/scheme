// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.IO;
using Xunit;

using CShark.Lexer;
namespace CSharkTests.Lexer
{
    public class UtilLexerTests
    {
        static bool IsVariableChar(char c) {
            return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || ( c == '_');
        }
            
        [Fact]
        public void CreateTillConditionLexer() 
        {
            TillConditionLexer lexer = new TillConditionLexer( c => c == ' ' || c == '\t', "Space or what", false, TokenType.Ignorable);
            Assert.True(lexer != null);
        }

        [Fact]
        public void WhileLexer_VariableDelimiter_ScanSuccess()
        {
            WhileLexer lexer = new WhileLexer(c => IsVariableChar(c), "variable", TokenType.Variable);
            string[] variables = new string[] { "hello_a123", "HELLO", "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                                "abcdefghijklmnopqrstuvwxyz", "_", "0123456789","Z","A",
                                                "z", "a", "0" };
            char[] delims = new char[] { ';', '?', ' ', '\t', '\r', '\n', '$', ')', '(', '{', '}', '[', ']'};

            foreach(var name in variables)
            {
                foreach(var delim in delims)
                {
                    var inputStr = $"{name}{delim}";
                    using (IReader reader = new Reader(new StringReader(inputStr)))
                    {
                        Token t = lexer.Scan(reader);
                        Assert.NotNull(t);
                        Assert.Equal(1, t.Line);
                        Assert.Equal(name, t.Text);
                        Assert.True(reader.MoveNext());
                        Assert.Equal(delim, reader.Current);
                    }
                }
            }
        }

        [Fact]
        public void WhileLexer_VariableEOF_ScanSuccess()
        {
            WhileLexer lexer = new WhileLexer(c => IsVariableChar(c), "variable", TokenType.Variable);
            string[] variables = new string[] { "hello_a123", "HELLO", "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                                "abcdefghijklmnopqrstuvwxyz", "_", "0123456789","Z","A",
                                                "z", "a", "0" };

            foreach(var name in variables)
            {
                var inputStr = $"{name}";
                using (IReader reader = new Reader(new StringReader(inputStr)))
                {
                    Token t = lexer.Scan(reader);
                    Assert.NotNull(t);
                    Assert.Equal(1, t.Line);
                    Assert.Equal(name, t.Text);
                    Assert.False(reader.MoveNext());
                }
            }
        }
    }
}
