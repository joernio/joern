import { Link } from 'gatsby';
import React from 'react';
import sllogo from '../assets/images/shiftleft_logo.png';
import tubslogo from '../assets/images/tubs_logo.svg';
import ugologo from '../assets/images/ugo_logo.jpg';

const Footer = props => (
  <footer id="footer">
    <section>
      <h2>Primary Maintainer</h2>
	<p>
	<span className="image">
        <img src={sllogo} width="60%" style={{'background-color': 'white', 'padding' : '6px'}} />
	</span>
      </p>
    </section>
    <section>
      <h2>Past Maintainers</h2>
	<p>
        <span className="image">
        <img src={tubslogo} width="80%" style={{'background-color': 'white', 'padding' : '6px'}} />	
	</span>
	<span className="image">
	<img src={ugologo} width="50%" style={{'background-color': 'white', 'padding' : '6px'}} />
        </span>
        </p>
    </section>
  </footer>
);

export default Footer;
