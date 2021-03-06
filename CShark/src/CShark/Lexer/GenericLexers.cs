﻿// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.Text;
using System.Diagnostics;

namespace CShark.Lexer
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
            _isEndOptional = isEndOptional;
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

    class WhileLexer : ILexer
    {
        private readonly Predicate<char> _predicate;
        private readonly TokenType _tokenType;
        private string _description;

        public WhileLexer(Predicate<char> isEndChar, string description, TokenType tokenType)
        {
            _predicate = isEndChar;
            _tokenType = tokenType;
            _description = description;
        }

        public Token Scan(IReader reader)
        {
            var strb = new StringBuilder();
            strb.Append(reader.Current);
            int beginLine = reader.Line;
            int beginCol = reader.Column;
            while (reader.MoveNext())
            {
                if (!_predicate(reader.Current))
                {
                    reader.Revert(reader.Current);
                    break;
                }
                strb.Append(reader.Current);
            }

            return new Token(_tokenType, beginLine, beginCol, strb.ToString());
        }
    }

    /// <summary>
    ///   Lexer to scan tokens that are delimited by a pair of characters.
    ///   c1 c2 ..... c2 c1.
    ///   This lexer supports nested tokens.
    /// </summary>
    class PairDelimitedLexer : ILexer
    {
        private readonly char _first;
        private readonly char _second;
        private readonly TokenType _tokenType;
        private string _description;

        public PairDelimitedLexer(char first, char second, string description, TokenType tokenType)
        {
            _first = first;
            _second = second;
            _tokenType = tokenType;
            _description = description;
        }

        /// <summary>
        ///  assumption - when lexer is called, reader.Current == _second  
        /// </summary>
        public Token Scan(IReader reader)
        {
            Debug.Assert(reader.Current == _second, $"expected to be called when {_second} is Current but found {reader.Current}");
            var strb = new StringBuilder();
            int level = 1;
            int beginLine = reader.Line;
            int beginCol = reader.Column;
            while (reader.MoveNext())
            {
                if (reader.Current == _second)
                {
                    if (!reader.MoveNext())
                    {
                        throw new ScannerException($"Unexpected end of file. Expected {_second}. Runaway {_description} from ({beginLine}, {beginCol}");
                    }
                    if (reader.Current == _first)
                    {
                        level = level - 1;
                        if (level == 0)
                        {
                            return new Token(_tokenType, beginLine, beginCol, strb.ToString());
                        }
                        else
                        {
                            strb.Append(_second);
                            strb.Append(reader.Current);
                        }
                    }
                    else
                    {
                        strb.Append(_second);
                        strb.Append(reader.Current);
                    }
                }
                else
                {
                    strb.Append(reader.Current);
                    if (reader.Current == _first)
                    {
                        if (reader.MoveNext())
                        {
                            strb.Append(reader.Current);
                            if (reader.Current == _second)
                            {
                                level++;
                            }
                        }
                    }
                }
            }

            throw new ScannerException("Runaway " + _description, beginLine, beginCol);
        }
    }
}
