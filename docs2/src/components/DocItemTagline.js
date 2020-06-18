import React from 'react';

class DocItemTagline extends React.Component {
  render() {
    return (
      <div className="docItemTagline">
        {this.props.value}
      </div>
    );
  }
}

export default DocItemTagline;
