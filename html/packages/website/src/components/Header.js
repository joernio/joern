import React from 'react';
import Typed from 'typed.js';
import logo from '../assets/images/logo.svg';
import styled from 'styled-components';

const Terminal = styled.div`
  width: 600px;
  height: 300px;
  box-shadow: 0 22px 70px 4px #0000008f;
  border-radius: 5px 5px 0 0;
  margin: 0 auto;
`;

const TerminalHeader = styled.div`
  background: #555;
  border-radius: 5px 5px 0 0;
  display: flex;
  align-items: center;
  padding: 0.7rem;
`;

const TerminalCircle = styled.div`
  border-radius: 50px;
  width: 12px;
  height: 12px;
  margin: 0 0.2rem;
  &:nth-child(2) {
    background: #ffbd2e;
  }
  &:nth-child(1) {
    background: #ff5f57;
  }
  &:nth-child(3) {
    background: #2ace42;
  }
`;
const TerminalBody = styled.div`
  background: #000;
  height: 100%;
  border-radius: 0 0 5px 5px;
  color: #fff;
  display: flex;
  align-items: center;
  padding-left: 1rem;
  font-family: monospace, monospace;
  font-size: 0.8em;
  color: #111;
  border: none;
  width: min-content;
  word-break: break-all;
  width: 100%;
  overflow: auto;
  text-align: left;
  span {
    &[cmd] {
      color: #f8f8f2;
    }
    &[code] {
      color: #f92672;
    }
    &[code-1] {
      color: #f8f8f2;
    }
  }
`;

const Row = styled.div`
  display: flexbox;
  flex-direction: row;
  transition-delay: 0s;
  align-items: center;
  justify-content: center;
`;

const typedStrings = [
  '',
  '<p><div style="background: transparent;">' +
    '<pre>' +
    '<span cmd><b>ocular></b> loadCpg</span><span code>(</span><span style="color: #e6db74">&quot;my_cpg.bin.zip&quot;</span><span style="color: #f92672">)</span>\n' +
    '<span cmd><b>ocular></b> val</span> <span code-1>sources</span> <span code>=</span> <span style="color: #f8f8f2">cpg</span><span style="color: #f92672">.</span><span style="color: #a6e22e">annotation</span><span style="color: #f92672">.</span><span style="color: #a6e22e">name</span><span style="color: #f92672">(</span>\n &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="color: #e6db74">&quot;.*(CookieValue|PathVariable).*&quot;</span><span style="color: #f92672">).</span><span style="color: #a6e22e">parameter</span>\n' +
    '<span cmd><b>ocular></b> val</span> <span code-1>sinks</span> <span style="color: #f92672">=</span> <span style="color: #f8f8f2">cpg</span><span style="color: #f92672">.</span><span style="color: #a6e22e">method</span><span style="color: #f92672">.</span><span style="color: #a6e22e">parameter</span>\n' +
    '<span cmd><b>ocular></b> sinks</span><span code>.</span><span style="color: #a6e22e">reachableBy</span><span style="color: #f92672">(</span><span style="color: #f8f8f2">sources</span><span style="color: #f92672">).</span><span style="color: #a6e22e">flows</span><span style="color: #f92672">.</span><span style="color: #a6e22e">p</span>\n' +
    '</pre></div>\n</p>',
];

const Header = props => {
  const pref = React.useRef();
  React.useEffect(() => {
    const typed = new Typed(pref.current, {
      strings: typedStrings,
      typeSpeed: 50,
      backSpeed: 0,
      startDelay: 5,
      showCursor: false,
      contentType: 'html',
      loop: false,
    });
    return typed.destroy;
  }, []);
  return (
    <header id="header" className="alt">
      <Row>
        <div>
          <img src={logo} alt="Logo" />
          <p>
            Next Generation{' '}
            <b>
              <em> Code Analysis</em>
            </b>
            <br />
            engine for <a href="/c-cpp">C/C++</a>.
          </p>
        </div>
        <Terminal>
          <TerminalHeader>
            <TerminalCircle />
            <TerminalCircle />
            <TerminalCircle />
          </TerminalHeader>
          <TerminalBody>
            <div ref={pref}></div>
          </TerminalBody>
        </Terminal>
      </Row>
    </header>
  );
};

export default Header;
