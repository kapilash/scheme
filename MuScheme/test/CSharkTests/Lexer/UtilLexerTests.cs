// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using Xunit;

using MuScheme.Lexer;
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
    }
}
