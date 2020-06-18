import GitHubButton from 'react-github-btn';
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
  padding-left:530px;
`;

const TerminalCircle = styled.div`
 border-radius: 50px;
  width: 12px;
  height: 12px;
  margin: 0 0.2rem;
  &:nth-child(2) {
    background: grey;
  }
  &:nth-child(1) {
    background: grey;
  }
  &:nth-child(3) {
    background: grey;
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
  font-size: 0.7em;
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
    '<span cmd><b>joern></b> importCode</span><span code>(</span><span style="color: #e6db74">&quot;/path/to/php-src&quot;</span><span style="color: #f92672">)</span>\n' +
	'<span cmd><b>joern></b> <span style="color: #f8f8f2">cpg</span>.call<span style="color: #f92672">(</span><span style="color: #e6db74">&quot;zend_parse_parameters&quot;</span><span style="color: #f92672">)</span>\n' + '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.argument.ast.isIdentifier\n' +
	'<span cmd>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.filter<span style="color: #f92672">(</span>_.typ.fullName<span style="color: #f92672">(</span><span style="color: #e6db74">&quot;.*char.*&quot;</span><span style="color: #f92672">))</span></span>\n' +
	'<span cmd>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.newTagNode<span style="color: #f92672">(</span><span style="color: #e6db74">&quot;attacker-controlled-string&quot;</span><span style="color: #f92672">)</span>.store</span>\n' +
    '<span cmd><b>joern></b> run.commit</span>\n' +
	'<span cmd><b>joern></b> cpg.call<span style="color: #f92672">(</span><span style="color: #e6db74">&quot;strcpy&quot;</span><span style="color: #f92672">)</span>.argument<span style="color: #f92672">(</span>2<span style="color: #f92672">)</span></span>\n' +
	'<span cmd>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;.reachableBy<span style="color: #f92672">(</span>cpg.tag<span style="color: #f92672">(</span><span style="color: #e6db74">&quot;attacker-contr.*&quot;</span><span style="color: #f92672">)</span>.identifier<span style="color: #f92672">)</span></span><span code>.</span>l\n' +
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
            Open-Source{' '}
            <b>
              <em> Code Querying</em>
            </b>
            <br />
            Engine for C/C++.<br/>

	    <GitHubButton href="https://github.com/ShiftLeftSecurity/joern/subscription"
	    data-color-scheme="no-preference: light; light: dark; dark: light;"
	    data-icon="octicon-eye" data-size="large" data-show-count="true"
	    aria-label="Watch ShiftLeftSecurity/joern on GitHub">
	    Watch
	    </GitHubButton>

	    <GitHubButton href="https://github.com/ShiftLeftSecurity/joern"
	    data-color-scheme="no-preference: light; light: dark; dark: light;"
	    data-icon="octicon-star" data-size="large" data-show-count="true"
	    aria-label="Star ShiftLeftSecurity/joern on GitHub">
	    Star
	    </GitHubButton>

	    <GitHubButton href="https://github.com/ShiftLeftSecurity/joern/fork"
	    data-color-scheme="no-preference: light; light: dark; dark: light;"
	    data-icon="octicon-repo-forked" data-size="large" data-show-count="true"
	    aria-label="Fork ShiftLeftSecurity/joern on GitHub">
	    Fork
	    </GitHubButton>

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
