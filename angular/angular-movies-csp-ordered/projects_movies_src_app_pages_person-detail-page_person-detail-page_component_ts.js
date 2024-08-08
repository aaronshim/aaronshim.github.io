"use strict";(self.webpackChunkmovies=self.webpackChunkmovies||[]).push([["projects_movies_src_app_pages_person-detail-page_person-detail-page_component_ts"],{3499:(at,g,i)=>{i.r(g),i.d(g,{default:()=>ot});var s=i(6814),t=i(5879),d=i(1321),P=i(7979),h=i(4826),m=i(8312),y=i(8202),l=i(4664),c=i(7398),C=i(8965),M=i(9360),_=i(7728),O=i(7400),b=i(8407),S=i(9940);function u(...n){const a=(0,S.jO)(n);return a?(0,b.z)(u(...n),(0,O.Z)(a)):(0,M.e)((e,o)=>{(0,C.l)([e,...(0,_.k)(n)])(o)})}function Z(...n){return u(...n)}var w=i(2460),v=i(4082),f=i(9699),T=i(5176),p=i(1102),$=i(3665),D=i(1017),A=i(7782),k=i(9862);let R=(()=>{class n{constructor(){this.http=(0,t.f3M)(k.eN),this.getPerson=(e,o={append_to_response:"videos"})=>this.http.get((n=>`${[A.w,"person",n].join("/")}`)(e),{params:o})}static#t=this.\u0275fac=function(o){return new(o||n)};static#e=this.\u0275prov=t.Yz7({token:n,factory:n.\u0275fac,providedIn:"root"})}return n})();var U=i(2303);let z=(()=>{class n extends d.x{constructor(){(0,t.f3M)(t.ktI).onDestroy(()=>this.actionsF.destroy()),super(),this.actionsF=new m.AE,this.personResource=(0,t.f3M)(R),this.actions=this.actionsF.create(),this.fetchPerson=this.actions.fetchPerson,this.sortMovies=this.actions.sortMovies,this.personByIdCtx=e=>this.select((0,c.U)(({person:{value:o,loading:r}})=>({loading:r,value:(0,U.j)(o,e)}))),this.connect("person",this.actions.fetchPerson$.pipe((0,$.m)(e=>e,e=>this.personResource.getPerson(e).pipe((0,c.U)(o=>({value:(0,p.qv)([o],"id")})),(0,D.Q)()))),(e,o)=>{const r=(0,p.r$)(e?.person,o);return r.value=(0,p.r$)(e?.person?.value,r.value),r})}initialize(e){this.fetchPerson(e)}static#t=this.\u0275fac=function(o){return new(o||n)};static#e=this.\u0275prov=t.Yz7({token:n,factory:n.\u0275fac,providedIn:"root"})}return n})();var J=i(9456);function x(n){return(0,f.z)(n,{pathProp:"poster_path",dims:v.Ch,sizes:"(min-width: 600px) 21vw, 15vw",srcset:"185w, 342w"})}let L=(()=>{class n extends d.x{constructor(){super(),this.routerState=(0,t.f3M)(y.u),this.personState=(0,t.f3M)(z),this.discoverResource=(0,t.f3M)(J.H),this.actions=(new m.AE).create(),this.paginate=this.actions.paginate,this.toggleSorting=this.actions.toggleSorting,this.sortBy=this.actions.sortBy,this.routerPersonId$=this.routerState.select((0,T.p)("person","detail")),this.sortingModel$=this.select((0,P.Cc)(["showSorting","activeSorting"])),this.routedPersonCtx$=this.routerPersonId$.pipe((0,l.w)(this.personState.personByIdCtx),(0,c.U)(e=>(e.value&&(e.value=function B(n){return(0,f.z)(n,{pathProp:"profile_path",dims:v.Ch,sizes:"(min-width: 901px) 15vw, 42vw",srcset:"154w, 185w, 342w, 500w, 780w"})}(e.value)),e))),this.movieRecommendationsById$=this.routerPersonId$.pipe(Z(this.routerState.select("sortBy")),(0,l.w)(([e,o])=>(0,h.h)(r=>this.discoverResource.getDiscoverMovies({with_cast:e,...r,sort_by:o}),this.actions.paginate$,this.discoverResource.getDiscoverMovies({with_cast:e,page:1,sort_by:o}))),(0,c.U)(e=>({...e,results:e.results?.map(x)}))),this.sortingEvent$=this.actions.sortBy$.pipe((0,w.M)(this.routerPersonId$),(0,l.w)(([{value:e},o])=>(0,h.h)(r=>this.discoverResource.getDiscoverMovies({with_cast:o,...r,sort_by:e}),this.actions.paginate$,this.discoverResource.getDiscoverMovies({with_cast:o,page:1,sort_by:e}))),(0,c.U)(e=>({...e,results:e.results?.map(x)}))),this.connect("showSorting",this.actions.toggleSorting$),this.connect(this.actions.sortBy$,(e,o)=>({showSorting:!1,activeSorting:o.name})),this.hold(this.routerPersonId$,this.personState.fetchPerson)}static#t=this.\u0275fac=function(o){return new(o||n)};static#e=this.\u0275prov=t.Yz7({token:n,factory:n.\u0275fac,providedIn:"root"})}return n})();const Q=[{name:"Popularity",value:"popularity.desc"},{name:"Votes Average",value:"vote_average.desc"},{name:"Original Title",value:"original_title.desc"},{name:"Release Date",value:"release_date.desc"}];var N=i(3019),F=i(3251),E=i(9542),Y=i(1780),j=i(5559);function H(n,a){if(1&n&&(t.TgZ(0,"a",13),t._uU(1," IMDB "),t._UZ(2,"fast-svg",14),t.qZA()),2&n){const e=t.oxw().ngIf;t.Q6J("href","https://www.imdb.com/person/"+e.imdb_id,t.LSH)}}function G(n,a){if(1&n){const e=t.EpF();t.ynx(0),t.TgZ(1,"ui-detail-grid")(2,"div",6),t._UZ(3,"img",7),t.qZA(),t.TgZ(4,"div",8)(5,"header")(6,"h1"),t._uU(7),t.qZA(),t.TgZ(8,"h2"),t._uU(9),t.qZA()(),t.TgZ(10,"section")(11,"h3"),t._uU(12,"The Biography"),t.qZA(),t.TgZ(13,"p"),t._uU(14),t.qZA()(),t.TgZ(15,"section",9),t.YNc(16,H,3,1,"a",10),t.TgZ(17,"button",11),t.NdJ("click",function(){t.CHM(e);const r=t.oxw(2);return t.KtG(r.back())}),t._UZ(18,"fast-svg",12),t._uU(19,"\xa0 Back "),t.qZA()()()(),t.BQk()}if(2&n){const e=a.ngIf;t.xp6(3),t.Q6J("ngSrc",e.imgSrc)("ngSrcset",e.imgSrcset)("sizes",e.imgSizes)("width",e.imgWidth)("height",e.imgHeight)("title",null==e?null:e.name),t.xp6(4),t.Oqu(e.name),t.xp6(2),t.Oqu(e.birthday),t.xp6(5),t.Oqu(e.biography||"There is no synopsis available..."),t.xp6(2),t.Q6J("ngIf",e.imdb_id)}}function K(n,a){1&n&&t._UZ(0,"div",15)}function V(n,a){if(1&n&&(t.TgZ(0,"article"),t.YNc(1,G,20,10,"ng-container",4),t.YNc(2,K,1,0,"ng-template",null,5,t.W1O),t.qZA()),2&n){const e=a.$implicit,o=t.MAs(3);t.xp6(1),t.Q6J("ngIf",null==e?null:e.value)("ngIfElse",o)}}function W(n,a){if(1&n&&(t.TgZ(0,"header")(1,"h1"),t._uU(2),t.qZA(),t.TgZ(3,"h2"),t._uU(4,"Movies"),t.qZA()()),2&n){const e=a.$implicit;t.xp6(2),t.hij("",(null==e||null==e.value?null:e.value.name)||"..."," in")}}const X=function(n){return{selected:n}};function q(n,a){if(1&n){const e=t.EpF();t.TgZ(0,"li",21)(1,"button",22),t.NdJ("click",function(){const it=t.CHM(e).$implicit,rt=t.oxw(2);return t.KtG(rt.sortBy(it))}),t._uU(2),t.qZA()()}if(2&n){const e=a.$implicit,o=t.oxw().$implicit;t.Q6J("ngClass",t.VKq(2,X,e.name===o.activeSorting)),t.xp6(2),t.hij(" ",e.name," ")}}function tt(n,a){if(1&n){const e=t.EpF();t.TgZ(0,"div",16)(1,"input",17),t.NdJ("click",function(){t.CHM(e);const r=t.oxw();return t.KtG(r.toggleSorting(!0))}),t.qZA(),t.TgZ(2,"ul",18),t.YNc(3,q,3,4,"li",19),t.qZA(),t.TgZ(4,"button",20),t.NdJ("click",function(){t.CHM(e);const r=t.oxw();return t.KtG(r.toggleSorting(!1))}),t._uU(5," \xa0 "),t.qZA()()}if(2&n){const e=a.$implicit,o=t.oxw();t.xp6(1),t.Q6J("value",e.activeSorting),t.xp6(1),t.Q6J("hidden",!e.showSorting),t.xp6(1),t.Q6J("ngForOf",o.sortOptions),t.xp6(1),t.Q6J("hidden",!e.showSorting)}}function et(n,a){1&n&&t._UZ(0,"div",15)}function nt(n,a){if(1&n){const e=t.EpF();t.ynx(0),t.TgZ(1,"ui-movie-list",23),t.NdJ("paginate",function(){t.CHM(e);const r=t.oxw();return t.KtG(r.paginate())}),t.qZA(),t.YNc(2,et,1,0,"div",24),t.BQk()}if(2&n){const e=a.$implicit;t.xp6(1),t.Q6J("movies",e.results),t.xp6(1),t.Q6J("ngIf",e.loading)}}const ot=(()=>{class n{constructor(){this.adapter=(0,t.f3M)(L),this.location=(0,t.f3M)(s.Ye),this.sortOptions=Q,this.personCtx$=this.adapter.routedPersonCtx$,this.sortingModel$=this.adapter.sortingModel$,this.sortBy=this.adapter.sortBy,this.toggleSorting=this.adapter.toggleSorting,this.infiniteScrollRecommendations$=(0,N.T)(this.adapter.movieRecommendationsById$,this.adapter.sortingEvent$),this.adapter.set({activeSorting:this.sortOptions[0].name,showSorting:!1})}paginate(){this.adapter.paginate()}back(){this.location.back()}static#t=this.\u0275fac=function(o){return new(o||n)};static#e=this.\u0275cmp=t.Xpm({type:n,selectors:[["ct-person"]],standalone:!0,features:[t.jDz],decls:7,vars:5,consts:[[4,"rxLet"],["for","sort"],["class","select-wrapper",4,"rxLet"],[4,"rxLet","rxLetStrategy"],[4,"ngIf","ngIfElse"],["loading",""],["detailGridMedia",""],["priority","high","alt","poster movie","data-uf","hero-img",1,"aspectRatio-2-3","fit-cover",3,"ngSrc","ngSrcset","sizes","width","height","title"],["detailGridDescription",""],[1,"movie-detail--ad-section-links"],["class","btn","target","_blank","rel","noopener noreferrer",3,"href",4,"ngIf"],["aria-label","Back",1,"btn","primary-button",3,"click"],["name","back","size","1em"],["target","_blank","rel","noopener noreferrer",1,"btn",3,"href"],["name","imdb",1,"btn__icon"],[1,"loader"],[1,"select-wrapper"],["id","sort","type","text","readonly","",1,"select",3,"value","click"],[1,"options",3,"hidden"],["class","option",3,"ngClass",4,"ngFor","ngForOf"],[1,"select-wrapper-overlay",3,"hidden","click"],[1,"option",3,"ngClass"],[1,"functionality-only-button",3,"click"],[3,"movies","paginate"],["class","loader",4,"ngIf"]],template:function(o,r){1&o&&(t.YNc(0,V,4,2,"article",0),t.TgZ(1,"article"),t.YNc(2,W,5,1,"header",0),t.TgZ(3,"label",1),t._uU(4,"Sort"),t.qZA(),t.YNc(5,tt,6,4,"div",2),t.YNc(6,nt,3,2,"ng-container",3),t.qZA()),2&o&&(t.Q6J("rxLet",r.personCtx$),t.xp6(2),t.Q6J("rxLet",r.personCtx$),t.xp6(3),t.Q6J("rxLet",r.sortingModel$),t.xp6(1),t.Q6J("rxLet",r.infiniteScrollRecommendations$)("rxLetStrategy","immediate"))},dependencies:[s.ax,s.O5,s.mk,s.Zd,F.K,E.O,Y.U,j.kB],styles:['.btn[_ngcontent-%COMP%]{display:flex;align-items:center;justify-content:center;display:inline-flex;outline:none;padding:6px 16px;min-width:96px;min-height:48px;font-weight:400;font-size:var(--text-md);color:var(--palette-primary-dark);border:1px solid rgba(var(--palette-primary-main-rgb),.5);border-radius:var(--theme-borderRadius-m);box-shadow:none;background-color:transparent;cursor:pointer}.btn__icon[_ngcontent-%COMP%]{width:24px;height:24px;margin-left:8px}.primary-button[_ngcontent-%COMP%]{background:var(--palette-primary-main);color:var(--palette-primary-contrast-text)}.primary-button[_ngcontent-%COMP%]:hover{background-color:var(--palette-primary-light)}.functionality-only-button[_ngcontent-%COMP%]{background:none;border:none;display:block;text-align:left;height:inherit}.loader[_ngcontent-%COMP%]{display:grid;place-items:center;min-width:150px;min-height:150px;width:100%}.loader[_ngcontent-%COMP%]:after{content:" ";width:3rem;height:3rem;border-radius:var(--theme-borderRadius-circle);background-color:var(--palette-primary-dark);box-shadow:-5rem 0 0 var(--palette-primary-main);animation:_ngcontent-%COMP%_circle-classic 1s ease-in-out infinite alternate}.loader.center-v[_ngcontent-%COMP%]{position:absolute;top:50%;left:50%;transform:translate(-50%,-50%)}@keyframes _ngcontent-%COMP%_circle-classic{0%{opacity:.1;transform:rotate(0) scale(.5)}to{opacity:1;transform:rotate(360deg) scale(1.2)}}.aspectRatio-2-3[_ngcontent-%COMP%]{width:100%;display:block;height:auto;aspect-ratio:var(--theme-aspectRatio-2-3)}.aspectRatio-16-9[_ngcontent-%COMP%]{width:100%;display:block;height:auto;aspect-ratio:var(--theme-aspectRatio-16-9)}.aspectRatio-4-3[_ngcontent-%COMP%]{width:100%;display:block;height:auto;aspect-ratio:var(--theme-aspectRatio-4-3)}.fit-cover[_ngcontent-%COMP%]{width:100%;display:block;object-fit:cover;height:100%}[_nghost-%COMP%]{width:100%;display:block}@media only screen and (max-width: 1500px){.movie-detail--grid-item[_ngcontent-%COMP%]{padding:3rem}}.movie-detail--ad-section-links[_ngcontent-%COMP%]   .section--content[_ngcontent-%COMP%]{display:flex;margin-right:auto}.movie-detail--ad-section-links[_ngcontent-%COMP%]   .btn[_ngcontent-%COMP%]{margin-right:2rem}@media only screen and (max-width: 1300px){.movie-detail--ad-section-links[_ngcontent-%COMP%]   .btn[_ngcontent-%COMP%]{margin-right:1rem}}.movie-detail--ad-section-links[_ngcontent-%COMP%] > .btn[_ngcontent-%COMP%]:last-child{margin-right:0rem;float:right}.movie-detail--section[_ngcontent-%COMP%]{margin-bottom:3rem}.select-wrapper[_ngcontent-%COMP%]{position:relative;max-width:300px}.select-wrapper-overlay[_ngcontent-%COMP%]{position:fixed;inset:0;margin:auto;background:transparent;z-index:1000;box-shadow:var(--theme-shadow-dropdown)}.options[_ngcontent-%COMP%]{max-height:300px;overflow:auto;position:absolute;top:calc(100% + 8px);background:var(--palette-background-paper);border:1px solid var(--palette-divider);width:100%;z-index:2000}.option[_ngcontent-%COMP%]{font-size:var(--text-md);padding:0 16px;min-height:36px;line-height:36px;border-bottom:1px solid var(--palette-divider);cursor:pointer}.option.selected[_ngcontent-%COMP%], .option[_ngcontent-%COMP%]:hover{background:var(--palette-action-hover)}.option[_ngcontent-%COMP%]   .functionality-only-button[_ngcontent-%COMP%]{width:100%}'],changeDetection:0})}return n})()}}]);