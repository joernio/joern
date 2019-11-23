import React from 'react';

import logo from '../assets/images/logo.svg';

const Header = props => (
  <header id="header" className="alt">
    <span className="logo">
      <img src={logo} alt="" />
    </span>
    {/* <h1>Joern</h1> */}
    <p>
     Next Generation <b><em> Code Analysis</em></b>
      <br />
      engine for <a href="#">C/C++</a>.
    </p>
  </header>
);

export default Header;
