(self.webpackChunk_N_E=self.webpackChunk_N_E||[]).push([[61],{8649:function(e,t,n){Promise.resolve().then(n.bind(n,7827)),Promise.resolve().then(n.t.bind(n,5469,23))},7827:function(e,t,n){"use strict";function r(){return r=Object.assign?Object.assign.bind():function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e},r.apply(this,arguments)}n.d(t,{default:function(){return l}});var o=n(9585),a=n(9956);function l(e){let{locale:t,...n}=e;if(!t)throw new Error("Failed to determine locale in `NextIntlClientProvider`, please provide the `locale` prop explicitly.\n\nSee https://next-intl-docs.vercel.app/docs/configuration#locale");return o.createElement(a.P,r({locale:t},n))}},9484:function(e,t,n){"use strict";var r=n(9585).createContext(void 0);t.IntlContext=r},9956:function(e,t,n){"use strict";var r=n(9585),o=n(5153),a=n(9484);var l=function(e){return e&&e.__esModule?e:{default:e}}(r);t.P=function(e){let{children:t,defaultTranslationValues:n,formats:i,getMessageFallback:s,locale:c,messages:u,now:f,onError:d,timeZone:g}=e;const[p]=r.useState((()=>new Map)),v=r.useMemo((()=>({...o.initializeConfig({locale:c,defaultTranslationValues:n,formats:i,getMessageFallback:s,messages:u,now:f,onError:d,timeZone:g}),messageFormatCache:p})),[n,i,s,c,p,u,f,d,g]);return l.default.createElement(a.IntlContext.Provider,{value:v},t)}},5153:function(e,t){"use strict";function n(){for(var e=arguments.length,t=new Array(e),n=0;n<e;n++)t[n]=arguments[n];return t.filter(Boolean).join(".")}function r(e){return n(e.namespace,e.key)}function o(e){console.error(e)}t.defaultGetMessageFallback=r,t.defaultOnError=o,t.initializeConfig=function(e){let{getMessageFallback:t,messages:n,onError:a,...l}=e;return{...l,messages:n,onError:a||o,getMessageFallback:t||r}},t.joinPath=n}},function(e){e.O(0,[469,293,286,744],(function(){return t=8649,e(e.s=t);var t}));var t=e.O();_N_E=t}]);