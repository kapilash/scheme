// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;

namespace MuScheme.Lexer
{
    public class Token
    {
        private readonly TokenType _tokenType;
        private readonly int _line;
        private readonly int _column;
        private readonly string _text;

        public Token(TokenType tokenType, int line, int column, string text)
        {
            _tokenType = tokenType;
            _line = line;
            _column = column;
            _text = text;
        }

        public TokenType TokenType { get { return _tokenType; } }
        public int Line { get { return _line; } }
        public int Column { get { return _column; } }
        public string Text { get { return _text; } }

        public override string ToString()
        {
            return $"{_tokenType}[{_text}] at ({_line}, {_column})";
        }
    }
}
