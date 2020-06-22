import React, { Component } from 'react';
import Lightbox from 'react-image-lightbox';
import 'react-image-lightbox/style.css';

export default class ImageWithDialog extends Component {
  constructor(props) {
    super(props);
    this.state = {
      src: props.src,
      alt: props.alt,
      showAltTextInCaption: props.showAltTextInCaption,
      isOpen: false,
    };
  }

  render() {
    const { photoIndex, isOpen, src, alt, showAltTextInCaption } = this.state;
    const showCaption = true;

    return (
      <div style={{marginBottom: "2rem"}}>
          <img style={{cursor: "pointer"}} alt={alt} src={src} onClick={() => this.setState({ isOpen: true })} />
          {showCaption && (
            <div><span style={{textAlign: "center", width: "100%", display: "block", fontStyle: "italic"}}>{alt}</span></div>
          )}

        {isOpen && (
          <Lightbox
            mainSrc={src}
            imageCaption={alt}
            onCloseRequest={() => this.setState({ isOpen: false })}
          />
        )}
      </div>
    );
  }
}
