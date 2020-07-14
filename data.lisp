;;; 
;;; data.lisp
;;;
;;; **********************************************************************
;;; Copyright (c) 2020 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
;;;
;;; Revision history: See git repository.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Gnu Public License, version 2 or
;;; later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
;;; of this agreement.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; **********************************************************************

(in-package :cl-fourier-animation)

(defparameter *arrow-path*
  (make-path 0.0 0.015 0.85 0.015 0.85 0.1 1.0 0.0 0.85 -0.1 0.85 -0.015 0.0 -0.015
             0.0 0.015))

(defparameter *achtel-512*
  (mapcar #'(lambda (x) (apply #'complex x))
          '((-0.8734 0.5648) (-0.8736 0.5649) (-0.8738 0.5649) (-0.874 0.5649)
            (-0.8742 0.5649) (-0.8744 0.565) (-0.8746 0.565) (-0.8748 0.5651)
            (-0.875 0.5652) (-0.8752 0.5653) (-0.8754 0.5654) (-0.8756 0.5655)
            (-0.8757 0.5656) (-0.8759 0.5657) (-0.8761 0.5658) (-0.8762 0.5659)
            (-0.8763 0.5661) (-0.8765 0.5662) (-0.8766 0.5664) (-0.8767 0.5665)
            (-0.8769 0.5667) (-0.877 0.5669) (-0.8771 0.567) (-0.8771 0.5672)
            (-0.8772 0.5674) (-0.8773 0.5676) (-0.8774 0.5678) (-0.8774 0.568)
            (-0.8775 0.5682) (-0.8775 0.5684) (-0.8775 0.5686) (-0.8776 0.5688)
            (-0.8776 0.569) (-0.8776 0.5792) (-0.8776 0.5894) (-0.8776 0.5997)
            (-0.8776 0.6099) (-0.8776 0.6201) (-0.8776 0.6303) (-0.8776 0.6405)
            (-0.8776 0.6507) (-0.8776 0.661) (-0.8776 0.6712) (-0.8776 0.6814)
            (-0.8776 0.6916) (-0.8776 0.7018) (-0.8776 0.712) (-0.8776 0.7223)
            (-0.8776 0.7325) (-0.8776 0.7427) (-0.8776 0.7529) (-0.8776 0.7631)
            (-0.8776 0.7733) (-0.8776 0.7835) (-0.8776 0.7938) (-0.8776 0.804)
            (-0.8776 0.8142) (-0.8776 0.8244) (-0.8776 0.8346) (-0.8776 0.8448)
            (-0.8776 0.8551) (-0.8776 0.8653) (-0.8776 0.8755) (-0.8776 0.8857)
            (-0.8776 0.8959) (-0.8784 0.8953) (-0.8792 0.8948) (-0.88 0.8942)
            (-0.8809 0.8937) (-0.8817 0.8932) (-0.8826 0.8927) (-0.8835 0.8922)
            (-0.8844 0.8918) (-0.8853 0.8914) (-0.8863 0.8909) (-0.8872 0.8905)
            (-0.8882 0.8902) (-0.8891 0.8898) (-0.8901 0.8895) (-0.8911 0.8892)
            (-0.8921 0.8889) (-0.8931 0.8886) (-0.8941 0.8883) (-0.8952 0.8881)
            (-0.8962 0.8878) (-0.8972 0.8876) (-0.8983 0.8874) (-0.8994 0.8873)
            (-0.9004 0.8871) (-0.9015 0.887) (-0.9026 0.8869) (-0.9036 0.8868)
            (-0.9047 0.8867) (-0.9058 0.8866) (-0.9069 0.8866) (-0.908 0.8865)
            (-0.9091 0.8865) (-0.9107 0.8866) (-0.9123 0.8866) (-0.9139 0.8867)
            (-0.9155 0.8868) (-0.9171 0.8869) (-0.9186 0.8871) (-0.9202 0.8873)
            (-0.9218 0.8875) (-0.9234 0.8878) (-0.9249 0.8881) (-0.9265 0.8884)
            (-0.9281 0.8887) (-0.9296 0.8891) (-0.9311 0.8895) (-0.9327 0.8899)
            (-0.9342 0.8903) (-0.9357 0.8908) (-0.9372 0.8913) (-0.9387 0.8918)
            (-0.9402 0.8924) (-0.9417 0.893) (-0.9432 0.8936) (-0.9447 0.8942)
            (-0.9461 0.8948) (-0.9476 0.8955) (-0.949 0.8962) (-0.9505 0.8969)
            (-0.9519 0.8976) (-0.9533 0.8984) (-0.9547 0.8991) (-0.9561 0.8999)
            (-0.9574 0.9007) (-0.9593 0.9019) (-0.9611 0.903) (-0.963 0.9043)
            (-0.9649 0.9055) (-0.9668 0.9069) (-0.9686 0.9083) (-0.9705 0.9097)
            (-0.9724 0.9112) (-0.9742 0.9128) (-0.976 0.9144) (-0.9778 0.916)
            (-0.9795 0.9177) (-0.9812 0.9195) (-0.9829 0.9213) (-0.9845 0.9232)
            (-0.9861 0.9251) (-0.9876 0.927) (-0.989 0.9291) (-0.9904 0.9311)
            (-0.9917 0.9333) (-0.9929 0.9354) (-0.9941 0.9377) (-0.9951 0.94)
            (-0.9961 0.9423) (-0.997 0.9447) (-0.9978 0.9471) (-0.9984 0.9496)
            (-0.999 0.9522) (-0.9994 0.9548) (-0.9997 0.9574) (-0.9999 0.9601)
            (-1.0 0.9629) (-0.9999 0.9652) (-0.9997 0.9675) (-0.9994 0.9697)
            (-0.999 0.9718) (-0.9985 0.9738) (-0.9978 0.9757) (-0.997 0.9776)
            (-0.9962 0.9794) (-0.9952 0.9811) (-0.9942 0.9827) (-0.993 0.9843)
            (-0.9918 0.9858) (-0.9905 0.9872) (-0.9891 0.9885) (-0.9876 0.9898)
            (-0.9861 0.991) (-0.9845 0.9921) (-0.9828 0.9931) (-0.9811 0.9941)
            (-0.9793 0.995) (-0.9775 0.9958) (-0.9756 0.9965) (-0.9736 0.9972)
            (-0.9717 0.9978) (-0.9697 0.9983) (-0.9676 0.9987) (-0.9656 0.9991)
            (-0.9635 0.9994) (-0.9614 0.9997) (-0.9592 0.9999) (-0.9571 1.0) (-0.955 1.0)
            (-0.9534 1.0) (-0.9518 0.9999) (-0.9502 0.9999) (-0.9486 0.9997)
            (-0.947 0.9996) (-0.9454 0.9994) (-0.9438 0.9992) (-0.9422 0.999)
            (-0.9407 0.9988) (-0.9391 0.9985) (-0.9375 0.9982) (-0.936 0.9978)
            (-0.9344 0.9975) (-0.9329 0.9971) (-0.9313 0.9966) (-0.9298 0.9962)
            (-0.9283 0.9957) (-0.9268 0.9952) (-0.9253 0.9947) (-0.9238 0.9942)
            (-0.9223 0.9936) (-0.9208 0.993) (-0.9193 0.9924) (-0.9179 0.9917)
            (-0.9164 0.9911) (-0.915 0.9904) (-0.9135 0.9897) (-0.9121 0.989)
            (-0.9107 0.9882) (-0.9093 0.9874) (-0.9079 0.9866) (-0.9066 0.9858)
            (-0.9047 0.9847) (-0.9028 0.9835) (-0.901 0.9823) (-0.8991 0.981)
            (-0.8972 0.9797) (-0.8954 0.9783) (-0.8935 0.9769) (-0.8916 0.9754)
            (-0.8898 0.9738) (-0.888 0.9722) (-0.8862 0.9705) (-0.8845 0.9688)
            (-0.8828 0.9671) (-0.8811 0.9653) (-0.8795 0.9634) (-0.8779 0.9615)
            (-0.8764 0.9595) (-0.875 0.9575) (-0.8736 0.9554) (-0.8723 0.9533)
            (-0.8711 0.9511) (-0.8699 0.9489) (-0.8689 0.9466) (-0.8679 0.9442)
            (-0.867 0.9418) (-0.8663 0.9394) (-0.8656 0.9369) (-0.865 0.9344)
            (-0.8646 0.9318) (-0.8643 0.9291) (-0.8641 0.9264) (-0.864 0.9237)
            (-0.864 0.9134) (-0.864 0.9036) (-0.864 0.8943) (-0.864 0.8854)
            (-0.864 0.8768) (-0.864 0.8686) (-0.864 0.8608) (-0.864 0.8532)
            (-0.864 0.8458) (-0.864 0.8387) (-0.864 0.8318) (-0.864 0.825) (-0.864 0.8184)
            (-0.864 0.8119) (-0.864 0.8054) (-0.864 0.7989) (-0.864 0.7925) (-0.864 0.786)
            (-0.864 0.7794) (-0.864 0.7727) (-0.864 0.766) (-0.864 0.759) (-0.864 0.7518)
            (-0.864 0.7445) (-0.864 0.7368) (-0.864 0.7289) (-0.864 0.7206) (-0.864 0.712)
            (-0.864 0.703) (-0.864 0.6935) (-0.864 0.6837) (-0.864 0.6733)
            (-0.8612 0.6762) (-0.8583 0.6793) (-0.8554 0.6824) (-0.8525 0.6856)
            (-0.8496 0.6888) (-0.8466 0.6921) (-0.8437 0.6955) (-0.8407 0.6989)
            (-0.8378 0.7023) (-0.8349 0.7059) (-0.832 0.7094) (-0.8292 0.7131)
            (-0.8264 0.7167) (-0.8237 0.7204) (-0.821 0.7242) (-0.8185 0.7279)
            (-0.816 0.7317) (-0.8136 0.7356) (-0.8113 0.7395) (-0.8091 0.7434)
            (-0.8071 0.7473) (-0.8051 0.7512) (-0.8034 0.7552) (-0.8017 0.7592)
            (-0.8003 0.7632) (-0.799 0.7672) (-0.7978 0.7712) (-0.7969 0.7752)
            (-0.7961 0.7792) (-0.7956 0.7833) (-0.7953 0.7873) (-0.7951 0.7913)
            (-0.7952 0.7939) (-0.7952 0.7965) (-0.7953 0.7991) (-0.7954 0.8016)
            (-0.7956 0.8042) (-0.7958 0.8068) (-0.7961 0.8093) (-0.7964 0.8119)
            (-0.7967 0.8144) (-0.797 0.817) (-0.7974 0.8195) (-0.7978 0.8221)
            (-0.7983 0.8246) (-0.7988 0.8271) (-0.7993 0.8296) (-0.7998 0.8322)
            (-0.8004 0.8347) (-0.801 0.8372) (-0.8017 0.8397) (-0.8024 0.8422)
            (-0.8031 0.8446) (-0.8038 0.8471) (-0.8046 0.8496) (-0.8054 0.852)
            (-0.8062 0.8545) (-0.807 0.8569) (-0.8079 0.8594) (-0.8088 0.8618)
            (-0.8097 0.8642) (-0.8107 0.8666) (-0.8117 0.869) (-0.8127 0.8714)
            (-0.8126 0.8725) (-0.8125 0.8735) (-0.8123 0.8744) (-0.812 0.8753)
            (-0.8117 0.8762) (-0.8114 0.877) (-0.811 0.8777) (-0.8105 0.8784)
            (-0.81 0.879) (-0.8095 0.8796) (-0.8089 0.8801) (-0.8083 0.8806)
            (-0.8076 0.881) (-0.807 0.8814) (-0.8063 0.8817) (-0.8056 0.8819)
            (-0.8049 0.8821) (-0.8041 0.8822) (-0.8034 0.8823) (-0.8027 0.8823)
            (-0.8019 0.8822) (-0.8012 0.8821) (-0.8005 0.882) (-0.7997 0.8817)
            (-0.799 0.8814) (-0.7983 0.8811) (-0.7977 0.8807) (-0.797 0.8802)
            (-0.7964 0.8797) (-0.7958 0.8791) (-0.7953 0.8784) (-0.7947 0.8777)
            (-0.7937 0.8751) (-0.7928 0.8725) (-0.7918 0.8699) (-0.7909 0.8673)
            (-0.79 0.8646) (-0.7891 0.862) (-0.7883 0.8594) (-0.7875 0.8567)
            (-0.7867 0.8541) (-0.786 0.8514) (-0.7853 0.8487) (-0.7846 0.8461)
            (-0.7839 0.8434) (-0.7833 0.8407) (-0.7827 0.838) (-0.7821 0.8353)
            (-0.7816 0.8326) (-0.7811 0.8299) (-0.7806 0.8271) (-0.7802 0.8244)
            (-0.7798 0.8217) (-0.7794 0.8189) (-0.7791 0.8162) (-0.7788 0.8134)
            (-0.7785 0.8107) (-0.7783 0.8079) (-0.7781 0.8052) (-0.7779 0.8024)
            (-0.7778 0.7997) (-0.7777 0.7969) (-0.7777 0.7941) (-0.7776 0.7913)
            (-0.7779 0.7837) (-0.7786 0.7762) (-0.7798 0.7688) (-0.7813 0.7616)
            (-0.7833 0.7544) (-0.7856 0.7473) (-0.7882 0.7404) (-0.7911 0.7335)
            (-0.7943 0.7267) (-0.7977 0.7199) (-0.8012 0.7132) (-0.805 0.7065)
            (-0.8088 0.6999) (-0.8128 0.6933) (-0.8168 0.6867) (-0.8208 0.6802)
            (-0.8249 0.6736) (-0.8289 0.667) (-0.8328 0.6604) (-0.8367 0.6538)
            (-0.8404 0.6471) (-0.844 0.6404) (-0.8473 0.6337) (-0.8505 0.6269)
            (-0.8534 0.62) (-0.856 0.613) (-0.8583 0.6059) (-0.8603 0.5988)
            (-0.8619 0.5915) (-0.863 0.5841) (-0.8637 0.5766) (-0.864 0.569)
            (-0.864 0.5688) (-0.864 0.5686) (-0.864 0.5684) (-0.8641 0.5682)
            (-0.8641 0.568) (-0.8642 0.5678) (-0.8642 0.5676) (-0.8643 0.5674)
            (-0.8644 0.5672) (-0.8645 0.567) (-0.8646 0.5668) (-0.8647 0.5667)
            (-0.8648 0.5665) (-0.8649 0.5664) (-0.8651 0.5662) (-0.8652 0.5661)
            (-0.8653 0.5659) (-0.8655 0.5658) (-0.8657 0.5657) (-0.8658 0.5656)
            (-0.866 0.5654) (-0.8662 0.5654) (-0.8663 0.5653) (-0.8665 0.5652)
            (-0.8667 0.5651) (-0.8669 0.565) (-0.8671 0.565) (-0.8673 0.5649)
            (-0.8675 0.5649) (-0.8677 0.5649) (-0.8679 0.5649) (-0.8682 0.5648)
            (-0.8683 0.5648) (-0.8685 0.5648) (-0.8686 0.5648) (-0.8688 0.5648)
            (-0.869 0.5648) (-0.8691 0.5648) (-0.8693 0.5648) (-0.8695 0.5648)
            (-0.8696 0.5648) (-0.8698 0.5648) (-0.87 0.5648) (-0.8701 0.5648)
            (-0.8703 0.5648) (-0.8704 0.5648) (-0.8706 0.5648) (-0.8708 0.5648)
            (-0.8709 0.5648) (-0.8711 0.5648) (-0.8713 0.5648) (-0.8714 0.5648)
            (-0.8716 0.5648) (-0.8718 0.5648) (-0.8719 0.5648) (-0.8721 0.5648)
            (-0.8722 0.5648) (-0.8724 0.5648) (-0.8726 0.5648) (-0.8727 0.5648)
            (-0.8729 0.5648) (-0.8731 0.5648) (-0.8732 0.5648))))

(defparameter *hessen-512*
  (mapcar #'(lambda (x) (apply #'complex x))
          '((-0.4019 0.1754) (-0.4026 0.1875) (-0.3969 0.1988) (-0.3987 0.2036)
            (-0.3949 0.205) (-0.3903 0.2065) (-0.3897 0.2115) (-0.3842 0.2123)
            (-0.3787 0.2133) (-0.3732 0.2146) (-0.3679 0.2161) (-0.3698 0.2212)
            (-0.368 0.225) (-0.3648 0.2284) (-0.3628 0.2323) (-0.3623 0.237)
            (-0.358 0.2395) (-0.3378 0.2452) (-0.3202 0.2542) (-0.318 0.2572)
            (-0.3216 0.262) (-0.3246 0.2671) (-0.3247 0.2729) (-0.3244 0.2787)
            (-0.3263 0.2842) (-0.3293 0.2811) (-0.3308 0.2772) (-0.3325 0.2733)
            (-0.3362 0.2697) (-0.3418 0.2706) (-0.3465 0.2717) (-0.3481 0.2741)
            (-0.3447 0.279) (-0.3399 0.2818) (-0.3364 0.2838) (-0.3353 0.2863)
            (-0.3374 0.2904) (-0.3421 0.2989) (-0.3441 0.3079) (-0.3339 0.3115)
            (-0.3264 0.3173) (-0.3249 0.3212) (-0.3259 0.3249) (-0.3279 0.3289)
            (-0.3297 0.3335) (-0.3357 0.3331) (-0.3423 0.3337) (-0.3474 0.3326)
            (-0.3488 0.3273) (-0.3621 0.328) (-0.374 0.329) (-0.3765 0.3345)
            (-0.3755 0.3394) (-0.371 0.3476) (-0.3707 0.3554) (-0.3747 0.3575)
            (-0.3805 0.3576) (-0.3858 0.357) (-0.3911 0.3554) (-0.3952 0.3556)
            (-0.3968 0.3605) (-0.3937 0.3616) (-0.3894 0.3607) (-0.3873 0.3612)
            (-0.3906 0.3663) (-0.3902 0.3706) (-0.3873 0.3683) (-0.3831 0.3649)
            (-0.3788 0.3657) (-0.3742 0.3685) (-0.3715 0.3727) (-0.3695 0.3767)
            (-0.3669 0.3785) (-0.3691 0.3836) (-0.3736 0.388) (-0.3762 0.3916)
            (-0.3722 0.3946) (-0.374 0.3993) (-0.3777 0.4012) (-0.3874 0.4035)
            (-0.395 0.4063) (-0.3971 0.4105) (-0.3988 0.415) (-0.3977 0.4212)
            (-0.3985 0.4241) (-0.4007 0.4256) (-0.4041 0.4279) (-0.4025 0.4321)
            (-0.4009 0.437) (-0.4032 0.4469) (-0.407 0.456) (-0.4074 0.4601)
            (-0.4103 0.4636) (-0.415 0.4671) (-0.4166 0.4725) (-0.4167 0.4786)
            (-0.4166 0.4842) (-0.4114 0.4831) (-0.4066 0.4844) (-0.4016 0.4862)
            (-0.3961 0.4867) (-0.3941 0.4825) (-0.3961 0.4777) (-0.3974 0.4737)
            (-0.393 0.4713) (-0.3882 0.4687) (-0.3833 0.4671) (-0.3783 0.4668)
            (-0.373 0.4684) (-0.3667 0.4704) (-0.3632 0.4762) (-0.3614 0.4835)
            (-0.3607 0.4901) (-0.3644 0.4901) (-0.3685 0.4919) (-0.3706 0.4954)
            (-0.3685 0.5002) (-0.3683 0.5141) (-0.3699 0.5261) (-0.3691 0.5323)
            (-0.3695 0.5379) (-0.3716 0.5436) (-0.376 0.55) (-0.3841 0.563)
            (-0.3963 0.5722) (-0.4107 0.5784) (-0.4254 0.5827) (-0.432 0.5792)
            (-0.4388 0.5754) (-0.4447 0.5753) (-0.4483 0.5826) (-0.4518 0.5909)
            (-0.4538 0.5995) (-0.4541 0.6082) (-0.4526 0.6172) (-0.4568 0.6261)
            (-0.4668 0.6304) (-0.4764 0.6351) (-0.4795 0.6451) (-0.4767 0.6499)
            (-0.4818 0.6531) (-0.49 0.6547) (-0.4968 0.655) (-0.5087 0.6522)
            (-0.52 0.6499) (-0.5194 0.6575) (-0.5214 0.6664) (-0.5216 0.6738)
            (-0.5156 0.6768) (-0.5156 0.682) (-0.5179 0.688) (-0.5194 0.6946)
            (-0.517 0.7013) (-0.524 0.7061) (-0.5338 0.7105) (-0.5436 0.7116)
            (-0.5508 0.7067) (-0.5546 0.6991) (-0.5612 0.6941) (-0.5693 0.6913)
            (-0.5772 0.6903) (-0.5934 0.6892) (-0.6083 0.6922) (-0.6089 0.6998)
            (-0.6142 0.7062) (-0.6191 0.708) (-0.6183 0.7018) (-0.6266 0.6978)
            (-0.6348 0.6987) (-0.6432 0.7019) (-0.6523 0.7045) (-0.6576 0.7095)
            (-0.6595 0.7165) (-0.6616 0.7231) (-0.6676 0.7269) (-0.6644 0.7309)
            (-0.6575 0.7317) (-0.6517 0.7339) (-0.6518 0.7418) (-0.6486 0.7497)
            (-0.648 0.7546) (-0.6497 0.7576) (-0.6529 0.7596) (-0.6539 0.771)
            (-0.6539 0.782) (-0.6521 0.7926) (-0.6478 0.8031) (-0.6474 0.8104)
            (-0.6444 0.8165) (-0.6401 0.8187) (-0.6356 0.8141) (-0.6361 0.821)
            (-0.6352 0.8287) (-0.6318 0.834) (-0.6249 0.8339) (-0.6223 0.8394)
            (-0.6259 0.846) (-0.6277 0.8518) (-0.6197 0.8548) (-0.6222 0.8609)
            (-0.6229 0.8682) (-0.6244 0.8749) (-0.629 0.8791) (-0.635 0.8825)
            (-0.6335 0.8897) (-0.6307 0.8976) (-0.6331 0.9031) (-0.6393 0.9084)
            (-0.6403 0.9134) (-0.6373 0.9182) (-0.6319 0.923) (-0.6307 0.9272)
            (-0.6309 0.9294) (-0.6362 0.9282) (-0.6406 0.9266) (-0.6367 0.9354)
            (-0.6294 0.9425) (-0.6254 0.9481) (-0.626 0.9543) (-0.633 0.9497)
            (-0.6397 0.9511) (-0.6464 0.9547) (-0.6539 0.9565) (-0.6608 0.9549)
            (-0.6675 0.9545) (-0.6734 0.9566) (-0.6781 0.9624) (-0.68 0.9675)
            (-0.6775 0.9736) (-0.6764 0.9794) (-0.6827 0.9832) (-0.6873 0.98)
            (-0.69 0.9844) (-0.693 0.992) (-0.6983 0.9989) (-0.7035 1.0) (-0.7091 0.9993)
            (-0.713 0.996) (-0.7132 0.9898) (-0.7083 0.9842) (-0.7078 0.975)
            (-0.7067 0.9672) (-0.7001 0.9655) (-0.6938 0.9665) (-0.6912 0.9599)
            (-0.6935 0.9538) (-0.702 0.9561) (-0.7089 0.9545) (-0.713 0.9508)
            (-0.7174 0.9477) (-0.7252 0.948) (-0.7354 0.9435) (-0.7434 0.9354)
            (-0.7473 0.9252) (-0.7451 0.9146) (-0.7487 0.909) (-0.7529 0.9068)
            (-0.7671 0.9095) (-0.772 0.9201) (-0.7691 0.9284) (-0.7676 0.9359)
            (-0.7709 0.942) (-0.7777 0.9454) (-0.785 0.9446) (-0.7898 0.9382)
            (-0.7955 0.9299) (-0.8034 0.9235) (-0.8126 0.9206) (-0.8223 0.9234)
            (-0.8277 0.9157) (-0.8325 0.9076) (-0.8361 0.899) (-0.8382 0.8898)
            (-0.8403 0.8807) (-0.8362 0.8734) (-0.8284 0.8686) (-0.8194 0.8669)
            (-0.8112 0.8574) (-0.8084 0.8473) (-0.8203 0.845) (-0.8282 0.8351)
            (-0.8315 0.826) (-0.8335 0.8175) (-0.8338 0.81) (-0.8393 0.8042)
            (-0.8439 0.7984) (-0.8418 0.7905) (-0.842 0.7784) (-0.8457 0.767)
            (-0.8524 0.7569) (-0.8615 0.7488) (-0.8695 0.7405) (-0.8796 0.7359)
            (-0.8907 0.7352) (-0.9015 0.7392) (-0.9193 0.7457) (-0.9367 0.7509)
            (-0.9456 0.756) (-0.9556 0.759) (-0.966 0.7599) (-0.9762 0.759)
            (-0.9853 0.7414) (-1.0 0.7293) (-0.9993 0.7231) (-0.9918 0.7186)
            (-0.984 0.7135) (-0.983 0.7054) (-0.9788 0.6997) (-0.9727 0.6986)
            (-0.9662 0.7008) (-0.9608 0.7047) (-0.9588 0.6963) (-0.9619 0.6903)
            (-0.9667 0.6847) (-0.9696 0.6775) (-0.9653 0.6706) (-0.9598 0.6657)
            (-0.9536 0.662) (-0.9469 0.6588) (-0.9424 0.6548) (-0.9375 0.6568)
            (-0.933 0.6585) (-0.9299 0.6538) (-0.9291 0.6478) (-0.9282 0.6418)
            (-0.9246 0.6384) (-0.9156 0.6398) (-0.9075 0.6384) (-0.9059 0.6326)
            (-0.9076 0.6248) (-0.9097 0.6173) (-0.9159 0.6127) (-0.919 0.6055)
            (-0.9261 0.5934) (-0.9369 0.5863) (-0.9431 0.5843) (-0.9463 0.5798)
            (-0.9385 0.5673) (-0.938 0.5565) (-0.94 0.5487) (-0.9405 0.54)
            (-0.9381 0.5324) (-0.9315 0.5279) (-0.927 0.521) (-0.9211 0.5227)
            (-0.9143 0.5271) (-0.9074 0.5286) (-0.9021 0.5239) (-0.8987 0.5173)
            (-0.8958 0.5101) (-0.8924 0.5032) (-0.8975 0.5) (-0.8992 0.494)
            (-0.9008 0.487) (-0.9057 0.4809) (-0.9019 0.467) (-0.896 0.4548)
            (-0.8909 0.449) (-0.8912 0.441) (-0.8947 0.433) (-0.8996 0.4272)
            (-0.9007 0.4219) (-0.895 0.4182) (-0.8881 0.414) (-0.886 0.4069)
            (-0.8725 0.3933) (-0.8585 0.3809) (-0.8535 0.3848) (-0.8483 0.3887)
            (-0.8347 0.3875) (-0.8262 0.376) (-0.8218 0.3698) (-0.8142 0.3663)
            (-0.8089 0.3642) (-0.8078 0.3572) (-0.8071 0.3488) (-0.803 0.3424)
            (-0.7916 0.3299) (-0.7886 0.3148) (-0.7891 0.3085) (-0.7918 0.3018)
            (-0.7933 0.296) (-0.7907 0.2922) (-0.7805 0.2917) (-0.77 0.2928)
            (-0.76 0.2934) (-0.7509 0.2918) (-0.7435 0.288) (-0.7438 0.2818)
            (-0.7447 0.2748) (-0.7392 0.2686) (-0.7331 0.263) (-0.7303 0.2562)
            (-0.7324 0.2399) (-0.7369 0.224) (-0.7403 0.2194) (-0.7474 0.2228)
            (-0.7551 0.2243) (-0.7622 0.2284) (-0.7684 0.2304) (-0.7733 0.2258)
            (-0.7782 0.2194) (-0.7766 0.212) (-0.7715 0.2047) (-0.7658 0.1984)
            (-0.7563 0.1883) (-0.7469 0.1791) (-0.7285 0.1748) (-0.7104 0.1725)
            (-0.7023 0.1744) (-0.6947 0.1714) (-0.687 0.1678) (-0.6782 0.168)
            (-0.6755 0.1614) (-0.6791 0.1575) (-0.6839 0.1533) (-0.6848 0.1455)
            (-0.6891 0.1375) (-0.6863 0.1307) (-0.6709 0.1225) (-0.6549 0.1212)
            (-0.6485 0.1244) (-0.6404 0.1262) (-0.6376 0.1334) (-0.6355 0.1413)
            (-0.6311 0.1473) (-0.6212 0.1486) (-0.615 0.1451) (-0.6084 0.1426)
            (-0.6028 0.1396) (-0.6002 0.1343) (-0.5961 0.1258) (-0.5875 0.1208)
            (-0.5788 0.1158) (-0.5746 0.1071) (-0.5689 0.1018) (-0.5646 0.0941)
            (-0.5632 0.0863) (-0.5665 0.0805) (-0.5566 0.0724) (-0.5416 0.0681)
            (-0.5392 0.0631) (-0.5335 0.0593) (-0.5277 0.0583) (-0.5244 0.0618)
            (-0.5224 0.0692) (-0.5165 0.0689) (-0.5097 0.0661) (-0.505 0.0662)
            (-0.5012 0.0704) (-0.4942 0.0692) (-0.4878 0.0687) (-0.4856 0.0745)
            (-0.4757 0.0841) (-0.4702 0.0971) (-0.4758 0.0965) (-0.4817 0.0944)
            (-0.4846 0.0946) (-0.4812 0.1012) (-0.4867 0.1008) (-0.4886 0.1042)
            (-0.4899 0.1093) (-0.4935 0.1142) (-0.4922 0.1265) (-0.4827 0.1359)
            (-0.4848 0.1413) (-0.4845 0.1476) (-0.4828 0.1542) (-0.4809 0.1609)
            (-0.4866 0.1645) (-0.4937 0.1651) (-0.5005 0.1662) (-0.5051 0.1717)
            (-0.5009 0.1769) (-0.5029 0.1796) (-0.5081 0.1804) (-0.5135 0.1798)
            (-0.4928 0.1921) (-0.4721 0.2007) (-0.4667 0.2049) (-0.4608 0.2102)
            (-0.4536 0.2074) (-0.4481 0.202) (-0.4447 0.1953) (-0.4439 0.1887)
            (-0.4494 0.1922) (-0.4549 0.1915) (-0.4597 0.1872) (-0.4628 0.1802)
            (-0.4558 0.1777) (-0.4506 0.1727) (-0.4453 0.1694) (-0.438 0.1721)
            (-0.439 0.1658) (-0.4368 0.1608) (-0.4328 0.1601) (-0.4283 0.1664)
            (-0.4247 0.1743) (-0.4189 0.1764) (-0.4159 0.1732) (-0.4205 0.165)
            (-0.4193 0.1608) (-0.4139 0.1576) (-0.4089 0.1576) (-0.4091 0.1628)
            (-0.4059 0.1667) (-0.4001 0.17) (-0.3971 0.1729) (-0.4019 0.1754)
            (-0.4019 0.1754) (-0.4019 0.1754) (-0.4019 0.1754))))