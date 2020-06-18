import React from 'react';

class StepHeader extends React.Component {
  render() {
    return (
      <div className="sl-step-header">
        <h3><span style={{color: "#9999A1"}}>Step {this.props.num}:</span> {this.props.title}</h3>
      </div>
    );
  }
}

export default StepHeader;
