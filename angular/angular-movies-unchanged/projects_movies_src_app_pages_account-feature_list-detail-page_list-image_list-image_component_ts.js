"use strict";
(self["webpackChunkmovies"] = self["webpackChunkmovies"] || []).push([["projects_movies_src_app_pages_account-feature_list-detail-page_list-image_list-image_component_ts"],{

/***/ 1061:
/*!***********************************************************************************************************!*\
  !*** ./projects/movies/src/app/pages/account-feature/list-detail-page/list-image/list-image.component.ts ***!
  \***********************************************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   "default": () => (__WEBPACK_DEFAULT_EXPORT__)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! @angular/core */ 1699);
/* harmony import */ var _shared_cdk_track_by__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ../../../../shared/cdk/track-by */ 5221);
/* harmony import */ var _list_detail_page_adapter__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../list-detail-page.adapter */ 2951);
/* harmony import */ var _rx_angular_template_for__WEBPACK_IMPORTED_MODULE_4__ = __webpack_require__(/*! @rx-angular/template/for */ 2788);
/* harmony import */ var _ui_component_grid_list_grid_list_component__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ../../../../ui/component/grid-list/grid-list.component */ 7866);
/* harmony import */ var _angular_common__WEBPACK_IMPORTED_MODULE_5__ = __webpack_require__(/*! @angular/common */ 6575);







function ListImageComponent_button_1_Template(rf, ctx) {
  if (rf & 1) {
    const _r4 = _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵgetCurrentView"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementStart"](0, "button", 2);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵlistener"]("click", function ListImageComponent_button_1_Template_button_click_0_listener() {
      const restoredCtx = _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵrestoreView"](_r4);
      const movie_r1 = restoredCtx.$implicit;
      const ctx_r3 = _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵnextContext"]();
      return _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵresetView"](ctx_r3.adapter.ui.listPosterUpdate(movie_r1.backdrop_path));
    });
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelement"](1, "img", 3);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementStart"](2, "div", 4)(3, "h3", 5);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtext"](4);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementStart"](5, "h2", 6);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtext"](6);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementEnd"]()()()();
  }
  if (rf & 2) {
    const movie_r1 = ctx.$implicit;
    const idx_r2 = ctx.index;
    const ctx_r0 = _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵnextContext"]();
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵclassProp"]("selected", movie_r1.selected);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵattribute"]("data-uf", "movie-" + idx_r2);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵadvance"](1);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵproperty"]("ngSrc", movie_r1.imgSrc)("ngSrcset", ctx_r0.adapter.srcset)("title", movie_r1.title);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵadvance"](3);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtextInterpolate1"](" ", movie_r1.title, " ");
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵadvance"](2);
    _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtextInterpolate1"](" ", movie_r1.selected ? "Selected" : "Select", " ");
  }
}
class ListImageComponent {
  constructor() {
    this.adapter = (0,_angular_core__WEBPACK_IMPORTED_MODULE_3__.inject)(_list_detail_page_adapter__WEBPACK_IMPORTED_MODULE_1__.ListDetailAdapter);
    this.trackByPosterId = (0,_shared_cdk_track_by__WEBPACK_IMPORTED_MODULE_0__.trackByProp)('id');
  }
  static #_ = this.ɵfac = function ListImageComponent_Factory(t) {
    return new (t || ListImageComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵdefineComponent"]({
    type: ListImageComponent,
    selectors: [["ct-list-image"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵStandaloneFeature"]],
    decls: 2,
    vars: 2,
    consts: [[1, "movies-list--grid"], ["class", "ui-grid-list-item", 3, "selected", "click", 4, "rxFor", "rxForOf", "rxForTrackBy"], [1, "ui-grid-list-item", 3, "click"], ["width", "355", "height", "200", "alt", "poster movie", 1, "aspectRatio-4-3", "gradient", 3, "ngSrc", "ngSrcset", "title"], [1, "movies-list--details"], [1, "movies-list--details-title"], [1, "ribbon"]],
    template: function ListImageComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementStart"](0, "ui-grid-list", 0);
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵtemplate"](1, ListImageComponent_button_1_Template, 7, 8, "button", 1);
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵelementEnd"]();
      }
      if (rf & 2) {
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵadvance"](1);
        _angular_core__WEBPACK_IMPORTED_MODULE_3__["ɵɵproperty"]("rxForOf", ctx.adapter.posters$)("rxForTrackBy", ctx.trackByPosterId);
      }
    },
    dependencies: [_rx_angular_template_for__WEBPACK_IMPORTED_MODULE_4__.RxFor, _ui_component_grid_list_grid_list_component__WEBPACK_IMPORTED_MODULE_2__.GridListComponent, _angular_common__WEBPACK_IMPORTED_MODULE_5__.NgOptimizedImage],
    styles: [".movies-list--details[_ngcontent-%COMP%] {\n  display: flex;\n  align-items: center;\n  flex-direction: column;\n  justify-content: space-between;\n  padding: 1.5rem 3rem;\n}\n@media only screen and (max-width: 500px) {\n  .movies-list--details[_ngcontent-%COMP%] {\n    padding: 1.5rem;\n  }\n}\n.movies-list--details-title[_ngcontent-%COMP%] {\n  text-align: center;\n  margin-bottom: 1rem;\n  line-height: 1.4;\n}\n\n.ui-grid-list-item[_ngcontent-%COMP%] {\n  content-visibility: auto;\n  position: relative;\n  cursor: pointer;\n}\n.ui-grid-list-item[_ngcontent-%COMP%]   img[_ngcontent-%COMP%] {\n  transition: filter 150ms ease-in-out;\n}\n.ui-grid-list-item[_ngcontent-%COMP%]:hover   .ribbon[_ngcontent-%COMP%] {\n  display: block;\n}\n.ui-grid-list-item[_ngcontent-%COMP%]:hover   img[_ngcontent-%COMP%] {\n  filter: grayscale(1);\n}\n.ui-grid-list-item.selected[_ngcontent-%COMP%]   .ribbon[_ngcontent-%COMP%] {\n  display: block;\n}\n\n.ribbon[_ngcontent-%COMP%] {\n  display: none;\n  position: absolute;\n  top: 0;\n  left: 0;\n  bottom: 0;\n  height: 40px;\n  line-height: 40px;\n  width: 100%;\n  text-align: center;\n  margin: auto;\n  background: var(--palette-primary-main);\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3BhZ2VzL2FjY291bnQtZmVhdHVyZS9saXN0LWRldGFpbC1wYWdlL2xpc3QtaW1hZ2UvbGlzdC1pbWFnZS5jb21wb25lbnQuc2NzcyIsIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL3Rva2VuL21peGlucy9fZmxleC5zY3NzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiJBQUdFO0VDS0EsYUFBQTtFQUNBLG1CQUFBO0VESkUsc0JBQUE7RUFDQSw4QkFBQTtFQUNBLG9CQUFBO0FBREo7QUFFSTtFQUxGO0lBTUksZUFBQTtFQUNKO0FBQ0Y7QUFDSTtFQUNFLGtCQUFBO0VBQ0EsbUJBQUE7RUFDQSxnQkFBQTtBQUNOOztBQUlBO0VBQ0Usd0JBQUE7RUFDQSxrQkFBQTtFQUNBLGVBQUE7QUFERjtBQUdFO0VBQ0Usb0NBQUE7QUFESjtBQUtJO0VBQ0UsY0FBQTtBQUhOO0FBTUk7RUFDRSxvQkFBQTtBQUpOO0FBUUU7RUFDRSxjQUFBO0FBTko7O0FBVUE7RUFDRSxhQUFBO0VBQ0Esa0JBQUE7RUFDQSxNQUFBO0VBQ0EsT0FBQTtFQUNBLFNBQUE7RUFDQSxZQUFBO0VBQ0EsaUJBQUE7RUFDQSxXQUFBO0VBQ0Esa0JBQUE7RUFDQSxZQUFBO0VBQ0EsdUNBQUE7QUFQRiIsInNvdXJjZXNDb250ZW50IjpbIkBpbXBvcnQgJy4uLy4uLy4uLy4uL3VpL3Rva2VuL21peGlucy9mbGV4JztcblxuLm1vdmllcy1saXN0IHtcbiAgJi0tZGV0YWlscyB7XG4gICAgQGluY2x1ZGUgZC1mbGV4LXY7XG4gICAgZmxleC1kaXJlY3Rpb246IGNvbHVtbjtcbiAgICBqdXN0aWZ5LWNvbnRlbnQ6IHNwYWNlLWJldHdlZW47XG4gICAgcGFkZGluZzogMS41cmVtIDNyZW07XG4gICAgQG1lZGlhIG9ubHkgc2NyZWVuIGFuZCAobWF4LXdpZHRoOiA1MDBweCkge1xuICAgICAgcGFkZGluZzogMS41cmVtO1xuICAgIH1cblxuICAgICYtdGl0bGUge1xuICAgICAgdGV4dC1hbGlnbjogY2VudGVyO1xuICAgICAgbWFyZ2luLWJvdHRvbTogMXJlbTtcbiAgICAgIGxpbmUtaGVpZ2h0OiAxLjQ7XG4gICAgfVxuICB9XG59XG5cbi51aS1ncmlkLWxpc3QtaXRlbSB7XG4gIGNvbnRlbnQtdmlzaWJpbGl0eTogYXV0bztcbiAgcG9zaXRpb246IHJlbGF0aXZlO1xuICBjdXJzb3I6IHBvaW50ZXI7XG5cbiAgaW1nIHtcbiAgICB0cmFuc2l0aW9uOiBmaWx0ZXIgMTUwbXMgZWFzZS1pbi1vdXQ7XG4gIH1cblxuICAmOmhvdmVyIHtcbiAgICAucmliYm9uIHtcbiAgICAgIGRpc3BsYXk6IGJsb2NrO1xuICAgIH1cblxuICAgIGltZyB7XG4gICAgICBmaWx0ZXI6IGdyYXlzY2FsZSgxKTtcbiAgICB9XG4gIH1cblxuICAmLnNlbGVjdGVkIC5yaWJib24ge1xuICAgIGRpc3BsYXk6IGJsb2NrO1xuICB9XG59XG5cbi5yaWJib24ge1xuICBkaXNwbGF5OiBub25lO1xuICBwb3NpdGlvbjogYWJzb2x1dGU7XG4gIHRvcDogMDtcbiAgbGVmdDogMDtcbiAgYm90dG9tOiAwO1xuICBoZWlnaHQ6IDQwcHg7XG4gIGxpbmUtaGVpZ2h0OiA0MHB4O1xuICB3aWR0aDogMTAwJTtcbiAgdGV4dC1hbGlnbjogY2VudGVyO1xuICBtYXJnaW46IGF1dG87XG4gIGJhY2tncm91bmQ6IHZhcigtLXBhbGV0dGUtcHJpbWFyeS1tYWluKTtcbn1cbiIsIkBtaXhpbiBkLWZsZXgge1xuICBkaXNwbGF5OiBmbGV4O1xufVxuQG1peGluIGQtaW5saW5lLWZsZXgge1xuICBkaXNwbGF5OiBpbmxpbmUtZmxleDtcbn1cblxuQG1peGluIGQtZmxleC12IHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbn1cblxuQG1peGluIGQtZmxleC1oIHtcbiAgZGlzcGxheTogZmxleDtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtdmgge1xuICBkaXNwbGF5OiBmbGV4O1xuICBhbGlnbi1pdGVtczogY2VudGVyO1xuICBqdXN0aWZ5LWNvbnRlbnQ6IGNlbnRlcjtcbn1cbiJdLCJzb3VyY2VSb290IjoiIn0= */"],
    changeDetection: 0
  });
}
/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (ListImageComponent);

/***/ }),

/***/ 7866:
/*!*******************************************************************************!*\
  !*** ./projects/movies/src/app/ui/component/grid-list/grid-list.component.ts ***!
  \*******************************************************************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   GridListComponent: () => (/* binding */ GridListComponent)
/* harmony export */ });
/* harmony import */ var _angular_core__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! @angular/core */ 1699);

const _c0 = [[["", 8, "ui-grid-list-item"]], "*"];
const _c1 = [".ui-grid-list-item", "*"];
class GridListComponent {
  static #_ = this.ɵfac = function GridListComponent_Factory(t) {
    return new (t || GridListComponent)();
  };
  static #_2 = this.ɵcmp = /*@__PURE__*/_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵdefineComponent"]({
    type: GridListComponent,
    selectors: [["ui-grid-list"]],
    standalone: true,
    features: [_angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵStandaloneFeature"]],
    ngContentSelectors: _c1,
    decls: 2,
    vars: 0,
    template: function GridListComponent_Template(rf, ctx) {
      if (rf & 1) {
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojectionDef"](_c0);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojection"](0);
        _angular_core__WEBPACK_IMPORTED_MODULE_0__["ɵɵprojection"](1, 1);
      }
    },
    styles: ["[_nghost-%COMP%] {\n  display: grid;\n  grid-template-columns: repeat(auto-fit, minmax(10rem, 35rem));\n  gap: 4rem 2rem;\n  place-content: space-between space-evenly;\n  align-items: start;\n  margin: 4rem 0;\n  position: relative;\n  contain: layout;\n}\n@media only screen and (max-width: 600px) {\n  [_nghost-%COMP%] {\n    grid-template-columns: repeat(auto-fit, minmax(10rem, 23rem));\n    justify-content: space-around;\n    gap: 4rem 1.5rem;\n  }\n}\n@media only screen and (max-width: 500px) {\n  [_nghost-%COMP%] {\n    grid-template-columns: repeat(auto-fit, minmax(10rem, 18rem));\n    gap: 4rem 1rem;\n  }\n}\n[_nghost-%COMP%]  .ui-grid-list-item {\n  display: flex;\n  align-items: stretch;\n  flex-direction: column;\n  transition: transform 150ms cubic-bezier(0.4, 0, 0.2, 1) 0s;\n}\n[_nghost-%COMP%]  .ui-grid-list-item:after {\n  content: \"\";\n  position: absolute;\n  z-index: -99;\n  top: 0px;\n  left: 0px;\n  width: 100%;\n  height: 100%;\n  transform: scaleY(0);\n  transform-origin: center top;\n  background-color: var(--palette-background-paper);\n  box-shadow: rgba(0, 0, 0, 0.2) 0 2px 1px -1px, rgba(0, 0, 0, 0.14) 0px 1px 1px 0px, rgba(0, 0, 0, 0.12) 0px 1px 3px 0px;\n}\n[_nghost-%COMP%]  .ui-grid-list-item:hover {\n  transform: scale(1.03);\n}\n[_nghost-%COMP%]  .ui-grid-list-item:hover:after {\n  transform: scale(1);\n}\n/*# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly8uL3Byb2plY3RzL21vdmllcy9zcmMvYXBwL3VpL2NvbXBvbmVudC9ncmlkLWxpc3QvZ3JpZC1saXN0LmNvbXBvbmVudC5zY3NzIiwid2VicGFjazovLy4vcHJvamVjdHMvbW92aWVzL3NyYy9hcHAvdWkvdG9rZW4vbWl4aW5zL19mbGV4LnNjc3MiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBRUE7RUFDRSxhQUFBO0VBQ0EsNkRBQUE7RUFDQSxjQUFBO0VBQ0EseUNBQUE7RUFDQSxrQkFBQTtFQUNBLGNBQUE7RUFDQSxrQkFBQTtFQUNBLGVBQUE7QUFERjtBQUdFO0VBVkY7SUFXSSw2REFBQTtJQUNBLDZCQUFBO0lBQ0EsZ0JBQUE7RUFBRjtBQUNGO0FBRUU7RUFoQkY7SUFpQkksNkRBQUE7SUFDQSxjQUFBO0VBQ0Y7QUFDRjtBQUVJO0VDdkJGLGFBQUE7RUR5Qkksb0JBQUE7RUFDQSxzQkFBQTtFQUNBLDJEQUFBO0FBQU47QUFFTTtFQUNFLFdBQUE7RUFDQSxrQkFBQTtFQUNBLFlBQUE7RUFDQSxRQUFBO0VBQ0EsU0FBQTtFQUNBLFdBQUE7RUFDQSxZQUFBO0VBQ0Esb0JBQUE7RUFDQSw0QkFBQTtFQUNBLGlEQUFBO0VBQ0EsdUhBQUE7QUFBUjtBQUlNO0VBQ0Usc0JBQUE7QUFGUjtBQUlRO0VBQ0UsbUJBQUE7QUFGViIsInNvdXJjZXNDb250ZW50IjpbIkBpbXBvcnQgJy4uLy4uLy4uL3VpL3Rva2VuL21peGlucy9mbGV4JztcblxuOmhvc3Qge1xuICBkaXNwbGF5OiBncmlkO1xuICBncmlkLXRlbXBsYXRlLWNvbHVtbnM6IHJlcGVhdChhdXRvLWZpdCwgbWlubWF4KDEwcmVtLCAzNXJlbSkpO1xuICBnYXA6IDRyZW0gMnJlbTtcbiAgcGxhY2UtY29udGVudDogc3BhY2UtYmV0d2VlbiBzcGFjZS1ldmVubHk7XG4gIGFsaWduLWl0ZW1zOiBzdGFydDtcbiAgbWFyZ2luOiA0cmVtIDA7XG4gIHBvc2l0aW9uOiByZWxhdGl2ZTtcbiAgY29udGFpbjogbGF5b3V0O1xuXG4gIEBtZWRpYSBvbmx5IHNjcmVlbiBhbmQgKG1heC13aWR0aDogNjAwcHgpIHtcbiAgICBncmlkLXRlbXBsYXRlLWNvbHVtbnM6IHJlcGVhdChhdXRvLWZpdCwgbWlubWF4KDEwcmVtLCAyM3JlbSkpO1xuICAgIGp1c3RpZnktY29udGVudDogc3BhY2UtYXJvdW5kO1xuICAgIGdhcDogNHJlbSAxLjVyZW07XG4gIH1cblxuICBAbWVkaWEgb25seSBzY3JlZW4gYW5kIChtYXgtd2lkdGg6IDUwMHB4KSB7XG4gICAgZ3JpZC10ZW1wbGF0ZS1jb2x1bW5zOiByZXBlYXQoYXV0by1maXQsIG1pbm1heCgxMHJlbSwgMThyZW0pKTtcbiAgICBnYXA6IDRyZW0gMXJlbTtcbiAgfVxuXG4gICY6Om5nLWRlZXAge1xuICAgIC51aS1ncmlkLWxpc3QtaXRlbSB7XG4gICAgICBAaW5jbHVkZSBkLWZsZXg7XG4gICAgICBhbGlnbi1pdGVtczogc3RyZXRjaDtcbiAgICAgIGZsZXgtZGlyZWN0aW9uOiBjb2x1bW47XG4gICAgICB0cmFuc2l0aW9uOiB0cmFuc2Zvcm0gMTUwbXMgY3ViaWMtYmV6aWVyKDAuNCwgMCwgMC4yLCAxKSAwcztcblxuICAgICAgJjphZnRlciB7XG4gICAgICAgIGNvbnRlbnQ6ICcnO1xuICAgICAgICBwb3NpdGlvbjogYWJzb2x1dGU7XG4gICAgICAgIHotaW5kZXg6IC05OTtcbiAgICAgICAgdG9wOiAwcHg7XG4gICAgICAgIGxlZnQ6IDBweDtcbiAgICAgICAgd2lkdGg6IDEwMCU7XG4gICAgICAgIGhlaWdodDogMTAwJTtcbiAgICAgICAgdHJhbnNmb3JtOiBzY2FsZVkoMCk7XG4gICAgICAgIHRyYW5zZm9ybS1vcmlnaW46IGNlbnRlciB0b3A7XG4gICAgICAgIGJhY2tncm91bmQtY29sb3I6IHZhcigtLXBhbGV0dGUtYmFja2dyb3VuZC1wYXBlcik7XG4gICAgICAgIGJveC1zaGFkb3c6IHJnYigwIDAgMCAvIDIwJSkgMCAycHggMXB4IC0xcHgsXG4gICAgICAgICAgcmdiKDAgMCAwIC8gMTQlKSAwcHggMXB4IDFweCAwcHgsIHJnYigwIDAgMCAvIDEyJSkgMHB4IDFweCAzcHggMHB4O1xuICAgICAgfVxuXG4gICAgICAmOmhvdmVyIHtcbiAgICAgICAgdHJhbnNmb3JtOiBzY2FsZSgxLjAzKTtcblxuICAgICAgICAmOmFmdGVyIHtcbiAgICAgICAgICB0cmFuc2Zvcm06IHNjYWxlKDEpO1xuICAgICAgICB9XG4gICAgICB9XG4gICAgfVxuICB9XG59XG4iLCJAbWl4aW4gZC1mbGV4IHtcbiAgZGlzcGxheTogZmxleDtcbn1cbkBtaXhpbiBkLWlubGluZS1mbGV4IHtcbiAgZGlzcGxheTogaW5saW5lLWZsZXg7XG59XG5cbkBtaXhpbiBkLWZsZXgtdiB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGFsaWduLWl0ZW1zOiBjZW50ZXI7XG59XG5cbkBtaXhpbiBkLWZsZXgtaCB7XG4gIGRpc3BsYXk6IGZsZXg7XG4gIGp1c3RpZnktY29udGVudDogY2VudGVyO1xufVxuXG5AbWl4aW4gZC1mbGV4LXZoIHtcbiAgZGlzcGxheTogZmxleDtcbiAgYWxpZ24taXRlbXM6IGNlbnRlcjtcbiAganVzdGlmeS1jb250ZW50OiBjZW50ZXI7XG59XG4iXSwic291cmNlUm9vdCI6IiJ9 */"],
    changeDetection: 0
  });
}


/***/ })

}]);
//# sourceMappingURL=projects_movies_src_app_pages_account-feature_list-detail-page_list-image_list-image_component_ts.js.map