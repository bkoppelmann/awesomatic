        subroutine crad(cxy,rusr,itri,ivert)
c
            integer itri(4,*),ivert(4,*)
            real rusr(2,*),cxy(3,*)
c
c       compute radii of curved edges...concave set to negative
c       ...convex set to positive
c
        nc=itri(3,4)
        if(nc.le.0) return
        i1=itri(1,2)
        i2=itri(2,2)-1
        do 11 i=i1,i2
            call cbound(i,ity,icen,mfi,mfe,itag,itri)
            if(icen.le.0) go to 11
            j2=(5-mfe)/2
            j3=6-j2-mfe
            j1=iabs(itri(mfe,mfi))+nc
            j2=iabs(itri(j2,mfi))+nc
            j3=iabs(itri(j3,mfi))+nc
            x=rusr(1,j1)
            y=rusr(2,j1)
            x2=rusr(1,j2)-x
            y2=rusr(2,j2)-y
            x3=rusr(1,j3)-x
            y3=rusr(2,j3)-y
            xc=rusr(1,icen)-x
            yc=rusr(2,icen)-y
            cxy(1,icen)=rusr(1,icen)
            cxy(2,icen)=rusr(2,icen)
            cxy(3,icen)=(x2-xc)*(x2-xc)+(y2-yc)*(y2-yc)
            call midpt(x2,y2,x3,y3,xc,yc,xm,ym)
            det=x2*y3-x3*y2
            c=1.0e0+(xm*(y2-y3)+ym*(x3-x2))/det
            if(c.gt.0.0e0) cxy(3,icen)=-cxy(3,icen)
  11        continue
        return
        end
