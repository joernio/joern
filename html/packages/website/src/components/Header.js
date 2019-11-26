import React from 'react';
import Typed from 'typed.js';
import logo from '../assets/images/logo.svg';

const Header = props => {
  const pref = React.useRef();
  React.useEffect(() => {
    const typed = new Typed(pref.current, {
      strings: ['First sentence.', 'Second sentence.'],
      typeSpeed: 10,
    });
    return typed.destroy;
  }, []);
  return (
    <header id="header" className="alt">
      <span className="logo">
        <img src={logo} alt="" />
      </span>
      <div ref={pref}></div>
      <p>
        Next Generation{' '}
        <b>
          <em> Code Analysis</em>
        </b>
        <br />
        engine for <a href="#">C/C++</a>.
      </p>
    </header>
  );
};

export default Header;
