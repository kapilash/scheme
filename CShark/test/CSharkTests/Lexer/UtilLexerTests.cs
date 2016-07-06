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
            WhileLexer lexer = new WhileLexer(c => IsVariableChar(c), "variable", TokenType.Identifier);
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
                        Assert.True(reader.MoveNext());
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
            WhileLexer lexer = new WhileLexer(c => IsVariableChar(c), "variable", TokenType.Identifier);
            string[] variables = new string[] { "hello_a123", "HELLO", "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                                "abcdefghijklmnopqrstuvwxyz", "_", "0123456789","Z","A",
                                                "z", "a", "0" };

            foreach(var name in variables)
            {
                var inputStr = $"{name}";
                using (IReader reader = new Reader(new StringReader(inputStr)))
                {
                    Assert.True(reader.MoveNext());
                    Token t = lexer.Scan(reader);
                    Assert.NotNull(t);
                    Assert.Equal(1, t.Line);
                    Assert.Equal(name, t.Text);
                    Assert.False(reader.MoveNext());
                }
            }
        }

        [Fact]
        public void PairDelimitedLexer_BlockComment_ScanSuccess()
        {
            ILexer lexer = new PairDelimitedLexer('/', '*', "block comment", TokenType.Ignorable);
            string comment = "ABCDEFGHIJKLMNOPQRSTUVWXYZ\n1234567890\n!@#$%^&*(){}':;'?/.<,~\n";
            string inputStr = $"*{comment}*/_";
            using (IReader reader = new Reader(new StringReader(inputStr)))
            {
                Assert.True(reader.MoveNext());
                Token t = lexer.Scan(reader);
                Assert.NotNull(t);
                Assert.Equal(TokenType.Ignorable, t.TokenType);
                Assert.Equal(1, t.Line);
                Assert.Equal(1, t.Column);
                Assert.Equal(comment, t.Text);
                Assert.True(reader.MoveNext());
                Assert.Equal('_', reader.Current);
                Assert.Equal(4, reader.Line);
            }
        }

        [Fact]
        public void PairDelimitedLexer_NestedBlockComment_ScanSuccess()
        {
            ILexer lexer = new PairDelimitedLexer('/', '*', "block comment", TokenType.Ignorable);
            string comment = "ABCDEFGHIJK/*LMNOPQRSTUVWXYZ\n1234567890\n!@#$%^&*(){}':;'?/.*/<,~\n";
            string inputStr = $"*{comment}*/_";
            using (IReader reader = new Reader(new StringReader(inputStr)))
            {
                Assert.True(reader.MoveNext());
                Token t = lexer.Scan(reader);
                Assert.NotNull(t);
                Assert.Equal(TokenType.Ignorable, t.TokenType);
                Assert.Equal(1, t.Line);
                Assert.Equal(1, t.Column);
                Assert.Equal(comment, t.Text);
                Assert.True(reader.MoveNext());
                Assert.Equal('_', reader.Current);
                Assert.Equal(4, reader.Line);
            }
        }

        [Fact]
        public void PairDelimitedLexer_BlockCommentEOF_ScanFailure()
        {
            ILexer lexer = new PairDelimitedLexer('/', '*', "block comment", TokenType.Ignorable);
            string comment = "ABCDEFGHIJKLMNOPQRSTUVWXYZ\n1234567890\n!@#$%^&*(){}':;'?/.<,~\n";
            string inputStr = $"*{comment}*_/";
            using (IReader reader = new Reader(new StringReader(inputStr)))
            {
                Assert.True(reader.MoveNext());
                Assert.Throws<ScannerException>(() => lexer.Scan(reader));
            }
        }

        [Fact]
        public void PairDelimitedLexer_NestedBlockCommentEOF_ScanFailure()
        {
            ILexer lexer = new PairDelimitedLexer('/', '*', "block comment", TokenType.Ignorable);
            string comment = "ABCDEFGHIJK/*LMNOPQRSTUVWXYZ\n1234567890\n!@#$%^&*(){}':;'?/.*/<,~\n";
            string inputStr = $"*{comment}*_/";
            using (IReader reader = new Reader(new StringReader(inputStr)))
            {
                Assert.True(reader.MoveNext());
                Assert.Throws<ScannerException>(() => lexer.Scan(reader));
            }
        }
    }
}
