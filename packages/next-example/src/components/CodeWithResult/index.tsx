import React from 'react';

const CodeWithResult: React.FC<{ children: React.ReactNode }> =
  ({ children }) => {
    return (<div className='pt-2 pb-2'>{children}</div>);
  }

export default CodeWithResult;
