pragma circom 2.1.6;

include "circomlib/poseidon.circom";  // Poseidon 해시 함수 포함

template HashProof() {
    // 신호 선언
    signal input preimage;      // 입력 값
    signal input hash;          // 주어진 해시 값
    signal output hashOutput;      // 유효성 검증 결과

    // Poseidon 해시 함수 선언
    component hasher = Poseidon(1);    // Poseidon 해시 함수에 1개의 입력 사용
    hasher.inputs[0] <== preimage;     // preimage 값을 Poseidon 해시 함수로 전달
    hashOutput <== hasher.out;

    // 해시 값 비교
    hashOutput === hash; // Poseidon 해시 값이 주어진 해시 값과 같은지 확인
}

// 메인 컴포넌트
component main = HashProof();

/* INPUT = {
    "preimage": "12345",
    "hash": "4267533774488295900887461483015112262021273608761099826938271132511348470966"
} */