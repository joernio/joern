import React from 'react';
import Scrollspy from 'react-scrollspy';
import Scroll from './Scroll';

const Nav = props => (
  <nav id="nav" className={props.sticky ? 'alt' : ''}>
    <Scrollspy
      items={['intro', 'first', 'second', 'cta']}
      currentClassName="is-active"
      offset={-300}
    >
      <li>
        <Scroll type="id" element="intro">
          <a href="/introduction">Introduction</a>
        </Scroll>
      </li>
      <li>
        <Scroll type="id" element="first">
          <a href="/introduction">Code Property Graphs</a>
        </Scroll>
      </li>
      <li>
        <Scroll type="id" element="second">
          <a href="/introduction">Getting Started</a>
        </Scroll>
      </li>
      <li>
        <Scroll type="id" element="cta">
          <a href="/introduction">Community</a>
        </Scroll>
      </li>
    </Scrollspy>
  </nav>
);

export default Nav;
