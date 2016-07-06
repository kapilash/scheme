// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.Text;
using System.Diagnostics;

namespace CShark.Lexer
{
    internal class CharLexer : ILexer
    {
        internal const string EscapedChars = "'\"\\0abfnrtv";
        internal static char[] EscapedVals = new char[] {
            '\'', '\"', '\\', '\0', '\a', '\b', '\f', '\n', '\r', '\t', '\v'
        };

        internal static char ReadNHexDigits(IReader reader, int nChars)
        {
            long code = 0;
            for (int i=0; i<nChars; i++)
            {
                if (reader.MoveNext())
                {
                    if ('0' <= reader.Current && '9' >= reader.Current)
                    {
                        code = (code * 16) + (reader.Current - '0');
                    }
                    else if ( 'A' <= reader.Current && 'F' >= reader.Current)
                    {
                        code = (code * 16) + 10 + (reader.Current - 'A');
                    }
                    else if ('a' <= reader.Current && 'f' >= reader.Current)
                    {
                        code = (code * 16) + 10 + (reader.Current - 'a');
                    }
                    else {
                        throw new ScannerException($"Unexpected {reader.Current}, expected hexadecimal", reader.Line, reader.Column);
                    }
                }
                else
                {
                    throw new ScannerException($"Unexpected EOF", reader.Line, reader.Column);
                }
            }

            return Convert.ToChar(code);
        }

        internal static char ScanChar(IReader reader)
        {
            if (reader.Current == '\\')
            {
                if (!reader.MoveNext())
                {
                    throw new ScannerException("Unexpected EOF", reader.Line, reader.Column);
                }

                for (int i=0; i < EscapedChars.Length; i++)
                {
                    if (EscapedChars[i] == reader.Current)
                        return EscapedVals[i];
                }

                if (reader.Current == 'u')
                {
                    return ReadNHexDigits(reader, 4);
                }

                if (reader.Current == 'U')
                {
                    return ReadNHexDigits(reader, 8);
                }

                throw new ScannerException($"Invalid Escaped character {reader.Current}", reader.Line, reader.Column);
            }

            return reader.Current;
        }

        public Token Scan(IReader reader)
        {

            int line = reader.Line;
            int column = reader.Column;
            if (!reader.MoveNext())
                throw new ScannerException("Unexpected EOF", line, column);
            char c = ScanChar(reader);
            if (reader.MoveNext() && reader.Current == '\'')
            {
                return new Token(TokenType.CharConstant, line, column, c);
            }

            throw new ScannerException("Invalid Char", line, column);
        }
    }
}
