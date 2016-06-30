using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;

namespace MuScheme.Lexer
{
    public abstract class Scanner : IEnumerator<Token>
    {
        private readonly List<ILexer> _lexers;
        private readonly ILexer _defaultLexer;
        private IReader _reader;
        private int _nextChar;
        private Token _nextToken;

        internal Scanner(IList<ILexer> lexers, ILexer defaultLexer, string text) : this(lexers, defaultLexer, new StringReader(text))
        {
        }

        internal Scanner(IList<ILexer> lexers, ILexer defaultLexer, Stream stream) : this(lexers, defaultLexer, new StreamReader(stream))
        {
        }

        private Scanner(IList<ILexer> lexers, ILexer defaultLexer, TextReader reader)
        {
            _lexers = new List<ILexer>();
            _lexers.AddRange(lexers);
            _defaultLexer = defaultLexer;
            _reader = new Reader(reader);
            _nextChar = -1;
            _nextToken = null;
        }

        public Token Current
        {
            get
            {
                return _nextToken;
            }
        }

        object IEnumerator.Current
        {
            get
            {
                return Current ;
            }
        }

        public void Dispose()
        {
            if (_reader != null)
                _reader.Dispose();
        }

        public bool MoveNext()
        {
            _nextToken = null;
            if (!_reader.MoveNext())
            {
                return false;
            }

            int current = Convert.ToInt32(_reader.Current);
            ILexer lexer = _defaultLexer;
            if (current < _lexers.Count)
            {
                lexer = _lexers[current];
            }
            _nextToken = lexer.Scan(_reader);
            return true;
        }

        public void Reset()
        {
            throw new NotImplementedException();
        }


    }
}
