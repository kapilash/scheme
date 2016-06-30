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
        private TextReader _reader;
        private int _nextChar;

        internal Scanner(IList<ILexer> lexers, string text)
        {
            _lexers = new List<ILexer>();
            _lexers.AddRange(lexers);
            _reader = new StringReader(text);
            _nextChar = -1;
        }

        internal Scanner(IList<ILexer> lexers, Stream stream)
        {
            _lexers = new List<ILexer>();
            _lexers.AddRange(lexers);
            _reader = new StreamReader(stream);
            _nextChar = -1;
        }

        public Token Current
        {
            get
            {
                throw new NotImplementedException();
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
            throw new NotImplementedException();
        }

        public void Reset()
        {
            throw new NotImplementedException();
        }


    }
}
