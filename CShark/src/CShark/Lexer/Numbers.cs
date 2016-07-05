// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.Text;
using System.Diagnostics;

namespace CShark.Lexer
{
    static class NumberLexer
    {
        internal static bool AppendTill(IReader reader, Predicate<char> predicate, StringBuilder builder)
        {
            do
            {
                if (predicate(reader.Current))
                {
                    builder.Append(reader.Current);
                }
                else
                {
                    reader.Revert(reader.Current);
                    return true;
                }
            } while (reader.MoveNext());

            return false;
        }

        /// <summary>
        ///   mantissa is of the form [0-9]*([eE][+-]?[0-9]+)?
        /// </summary>
        internal static double ReadMantissa(IReader reader, string prefix)
        {
            var builder = new StringBuilder();
            builder.Append(prefix);
            builder.Append('.');
            AppendTill(reader, (c) => c >= '0' && c <= '9', builder);
            if (reader.MoveNext())
            {
                if (reader.Current == 'e' || reader.Current == 'E')
                {
                    builder.Append('E');
                    if (reader.MoveNext())
                    {
                        if (reader.Current == '+' || reader.Current == '-')
                        {
                            builder.Append(reader.Current);
                            if (reader.MoveNext())
                            {
                                AppendTill(reader, (c) => c >= '0' && c <= '9', builder);
                            }
                        }
                        else
                        {
                            AppendTill(reader, (c) => c >= '0' && c <= '9', builder);
                        }
                    }
                }
                else
                {
                    AppendTill(reader, (c) => c >= '0' && c <= '9', builder);
                }
            }

            return Convert.ToDouble(builder.ToString());
        }

        internal static Token ContinueWithSuffix(IReader reader, int fromBase, int line, int column, string text)
        {
            switch (reader.Current)
            {
                case 'd':
                case 'D':
                    {
                        return new Token(TokenType.DoubleConstant, line, column, Convert.ToDouble(text));
                    }

                case 'f':
                case 'F':
                    {
                        return new Token(TokenType.FloatConstant, line, column, Convert.ToSingle(text));
                    }

                case 'l':
                case 'L':
                    {
                        if (reader.MoveNext())
                        {
                            if (reader.Current == 'u' || reader.Current == 'U')
                            {
                                return new Token(TokenType.ULongConstant, line, column, Convert.ToUInt64(text, fromBase));
                            }

                            reader.Revert(reader.Current);
                        }

                        return new Token(TokenType.LongConstant, line, column, Convert.ToInt64(text, fromBase));
                    }

                case 's':
                case 'S':
                    {
                        if (reader.MoveNext())
                        {
                            if (reader.Current == 'u' || reader.Current == 'U')
                            {
                                return new Token(TokenType.UShortConstant, line, column, Convert.ToUInt16(text, fromBase));
                            }

                            reader.Revert(reader.Current);
                        }

                        return new Token(TokenType.ShortConstant, line, column, Convert.ToInt16(text, fromBase));
                    }

                case 'u':
                case 'U':
                    {
                        if (reader.MoveNext())
                        {
                            if (reader.Current == 's' || reader.Current == 'S')
                            {
                                return new Token(TokenType.UShortConstant, line, column, Convert.ToUInt16(text, fromBase));
                            }

                            if (reader.Current == 'l' || reader.Current == 'L')
                            {
                                return new Token(TokenType.ULongConstant, line, column, Convert.ToUInt64(text, fromBase));
                            }
                            reader.Revert(reader.Current);
                        }

                        return new Token(TokenType.UIntConstant, line, column, Convert.ToUInt32(text, fromBase));
                    }
                    
                default:
                    {
                        reader.Revert(reader.Current);
                        break;
                    }
            }

            Console.WriteLine($"text is {text}");
            return new Lexer.Token(TokenType.IntConstant, line, column, Convert.ToInt32(text, fromBase));
        }

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
                    if (reader.Current == 'u' || reader.Current == 'U')
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
            while (reader.MoveNext())
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

    internal class ZeroLexer : ILexer
    {
        private Token ScanDouble(IReader reader, int line, int column)
        {
            double dbl = 0d;
            try
            {
                dbl = NumberLexer.ReadMantissa(reader, "0");
            }
            catch (FormatException f)
            {
                throw new ScannerException(f.Message, line, column);
            }

            if (reader.MoveNext())
            {
                if (reader.Current == 'f' || reader.Current == 'F')
                {
                    try
                    {
                        float f = Convert.ToSingle(dbl);
                        return new Token(TokenType.FloatConstant, line, column, Convert.ToSingle(dbl));
                    }
                    catch (FormatException f)
                    {
                        throw new ScannerException(f.Message, line, column);
                    }
                }
                if (reader.Current != 'd' && reader.Current != 'D')
                {
                    reader.Revert(reader.Current);
                }
            }

            return new Token(TokenType.DoubleConstant, line, column, dbl);
        }

        public Token Scan(IReader reader)
        {
            int line = reader.Line;
            int column = reader.Column;
            if (reader.MoveNext())
            {
                if (reader.Current == '.')
                {
                    return ScanDouble(reader, line, column);
                }
                else if (reader.Current == 'x' || reader.Current == 'X')
                {
                    var builder = new StringBuilder();
                    if (NumberLexer.AppendTill(reader, (c) => (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'), builder))
                    {
                        try
                        {
                            return NumberLexer.ContinueWithSuffix(reader, 16, line, column, builder.ToString());
                        }
                        catch (FormatException f)
                        {
                            throw new ScannerException(f.Message, line, column);
                        }
                    }
                    return new Token(TokenType.IntConstant, line, column, Convert.ToInt32(builder.ToString(), 16));
                }
                else
                {
                    reader.Revert(reader.Current);
                }
            }

            return new Token(TokenType.IntConstant, line, column, 0);
        }
    }

    public class Numbers
    {
    }
}
