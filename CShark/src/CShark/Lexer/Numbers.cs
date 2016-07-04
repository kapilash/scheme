// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.Text;
using System.Linq;
using System.Threading.Tasks;

namespace CShark.Lexer
{
    static class NumberLexer
    {
        private static TokenType ReadSuffix(IReader reader)
        {
            if (reader.Current == 'd' || reader.Current == 'D')
            {
                return TokenType.DoubleConstant;
            }

            if (reader.Current == 'f' || reader.Current == 'F')
            {
                return TokenType.FloatConstant;
            }

            if (reader.Current == 'l' || reader.Current == 'L')
            {
                if (reader.MoveNext())
                {
                    if(reader.Current == 'u' || reader.Current == 'U')
                    {
                        return TokenType.ULongConstant;
                    }

                    reader.Revert(reader.Current);
                }

                return TokenType.LongConstant;
            }

            if (reader.Current == 'u' || reader.Current == 'U')
            {
                if (reader.MoveNext())
                {
                    if (reader.Current == 's' || reader.Current == 'S')
                    {
                        return TokenType.UShortConstant;
                    }

                    if (reader.Current == 'l' || reader.Current == 'L')
                    {
                        return TokenType.ULongConstant;
                    }

                    reader.Revert(reader.Current);
                }

                return TokenType.UIntConstant;
            }

            if (reader.Current == 's' || reader.Current == 'S')
            {
                if (reader.MoveNext())
                {
                    if (reader.Current == 'u' || reader.Current == 'U')
                    {
                        return TokenType.UShortConstant;
                    }

                    reader.Revert(reader.Current);
                }

                return TokenType.ShortConstant;
            }

            reader.Revert(reader.Current);
            return TokenType.IntConstant;
        }

        public static TokenType ScanBinaryNumber(IReader reader, out UInt64 value)
        {
            value = (reader.Current == '0') ? (ulong)0 : (ulong)1;

            TokenType tokenType = TokenType.IntConstant;
            while (reader.MoveNext())
            {
                if (reader.Current == 1)
                {
                    value = value * 2 + 1;
                }
                else if (reader.Current == 0)
                {
                    value = value * 2;
                }
                else 
                {
                    tokenType = ReadSuffix(reader);
                    break;
                }
                value = value * 10 + Convert.ToUInt64(reader.Current - '0');
            }

            return tokenType;
        }


        public static TokenType ScanOctalNumber(IReader reader, out UInt64 value)
        {
            value = Convert.ToUInt64(reader.Current - '0');
            TokenType tokenType = TokenType.IntConstant;
            while (reader.MoveNext())
            {
                if (reader.Current > '8' || reader.Current < '0')
                {
                    tokenType = ReadSuffix(reader);
                    break;
                }

                value = value * 8 + Convert.ToUInt64(reader.Current - '0');
            }

            return tokenType;
        }

        public static TokenType ScanDecimal(IReader reader, out UInt64 value)
        {
            value = Convert.ToUInt64(reader.Current - '0');
            TokenType tokenType = TokenType.IntConstant;
            int line = reader.Line;
            int col = reader.Column;
            while(reader.MoveNext())
            {
                if (reader.Current > '9' || reader.Current < '0')
                {
                    tokenType = ReadSuffix(reader);
                    break;
                }
                value = value * 10 + Convert.ToUInt64(reader.Current - '0');
            }

            return tokenType;
        }

        private static bool CharToDigit(char c, out int value)
        {
            value = 0;
            if (c >= '0' && c <= '9')
            {
                value = c - '0';
                return true;
            }

            if (c >= 'A' && c <= 'F')
            {
                value = 10 + (c - 'A');
                return true;
            }

            if (c >= 'a' && c <= 'f')
            {
                value = 10 + (c - 'a');
                return true;
            }

            return false;
        }

        public static TokenType ScanHexadecimal(IReader reader, out UInt64 value)
        {
            int iv;
            CharToDigit(reader.Current, out iv);
            value = Convert.ToUInt64(iv);
            TokenType tokenType = TokenType.IntConstant;
            int line = reader.Line;
            int col = reader.Column;
            while (reader.MoveNext())
            {
                int digit;
                if (CharToDigit(reader.Current, out digit))
                {
                    value = value * 10 + Convert.ToUInt64(digit);
                }
                else
                {
                    tokenType = ReadSuffix(reader);
                }
            }

            return tokenType;
        }
    }

    public class Numbers
    {
    }
}
