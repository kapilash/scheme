// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.Text;

namespace CShark.Lexer
{
    class GenVariable : ILexer
    {
        private readonly Predicate<char> _isVariableChar;
        private readonly TokenType _tokenType;
        public GenVariable(Predicate<char> isVariableChar, TokenType tokenType)
        {
            _isVariableChar = isVariableChar;
            _tokenType = tokenType;
        }

        public Token Scan(IReader reader)
        {
            var strBuilder = new StringBuilder();
            strBuilder.Append(reader.Current);
            while(reader.MoveNext())
            {
                char c = reader.Current;
                if (!_isVariableChar(c))
                {
                    reader.Revert(c);
                    break;
                }
                strBuilder.Append(c);
            }

            return new Token(_tokenType, reader.Line, reader.Column, strBuilder.ToString());
        }
    }
}
