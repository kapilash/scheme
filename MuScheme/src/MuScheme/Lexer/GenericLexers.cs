using System;
using System.Text;

namespace MuScheme.Lexer
{
    class TillConditionLexer : ILexer
    {
        private readonly Predicate<char> _isEndChar;
        private bool _isEndOptional;
        private readonly TokenType _tokenType;
        private string _description;

        public TillConditionLexer(Predicate<char> isEndChar, string description, bool isEndOptional, TokenType tokenType)
        {
            _isEndChar = isEndChar;
            _tokenType = tokenType;
            isEndOptional = _isEndOptional;
            _description = description;
        }

        public Token Scan(IReader reader)
        {
            var strb = new StringBuilder(reader.Current);
            bool foundEnd = false;
            int beginLine = reader.Line;
            int beginCol = reader.Column;
            while (reader.MoveNext())
            {
                if (_isEndChar(reader.Current))
                {
                    foundEnd = true;
                    break;
                }
                strb.Append(reader.Current);
            }

            if(!foundEnd && !_isEndOptional)
            {
                throw new ScannerException("Runaway " + _description, beginLine, beginCol);
            }

            return new Token(_tokenType, beginLine, beginCol, strb.ToString());
        }
    }

    class TillPairLexer : ILexer
    {
        private readonly char _penultimate;
        private readonly char _final;
        private readonly TokenType _tokenType;
        private string _description;

        public TillPairLexer(char penultimate, char final, string description, TokenType tokenType)
        {
            _penultimate = penultimate;
            _final = final;
            _tokenType = tokenType;
            _description = description;
        }

        public Token Scan(IReader reader)
        {
            var strb = new StringBuilder(reader.Current);
            bool foundEnd = false;
            int beginLine = reader.Line;
            int beginCol = reader.Column;
            while (reader.MoveNext())
            {
                if (_isEndChar(reader.Current))
                {
                    foundEnd = true;
                    break;
                }
                strb.Append(reader.Current);
            }

            if (!foundEnd && !_isEndOptional)
            {
                throw new ScannerException("Runaway " + _description, beginLine, beginCol);
            }

            return new Token(_tokenType, beginLine, beginCol, strb.ToString());
        }
    }
}
