<?php


namespace prog\Datatypes;


interface IBaseType {

    public function getTypeId();
    public function setTypeId();
    public function getState();
    public function setState();
    public function getName();
    public function setName();
    public function getValue();
    public function setValue();
} 