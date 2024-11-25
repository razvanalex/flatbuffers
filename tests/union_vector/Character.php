<?php
// automatically generated by the FlatBuffers compiler, do not modify

use \Google\FlatBuffers\FlatBufferBuilder;

class CharacterT
{
    /**
     * @var Character $type
     */
    public $type;

    /**
     * @var mixed $value
     */
    public $value;

    /**
     * @param Character $type
     * @param mixed $value
     */
    public function __construct($type, $value)
    {
        $this->type = $type;
        $this->value = $value;
    }

    /**
     * @param FlatBufferBuilder $builder
     * @return int offset
     */
    public function pack(FlatBufferBuilder $builder)
    {
        switch ($this->type) {
            case Character::MuLan:
                return $this->value->pack($builder);
            case Character::Rapunzel:
                return $this->value->pack($builder);
            case Character::Belle:
                return $this->value->pack($builder);
            case Character::BookFan:
                return $this->value->pack($builder);
            case Character::Other:
                return $this->value13;
            case Character::Unused:
                return $this->value13;
            default:
                return 0;
        }
    }
}

class Character
{
    const NONE = 0;
    const MuLan = 1;
    const Rapunzel = 2;
    const Belle = 3;
    const BookFan = 4;
    const Other = 5;
    const Unused = 6;

    private static $names = array(
        Character::NONE=>"NONE",
        Character::MuLan=>"MuLan",
        Character::Rapunzel=>"Rapunzel",
        Character::Belle=>"Belle",
        Character::BookFan=>"BookFan",
        Character::Other=>"Other",
        Character::Unused=>"Unused",
    );

    public static function Name($e)
    {
        if (!isset(self::$names[$e])) {
            throw new \Exception();
        }
        return self::$names[$e];
    }

    /**
     * @return CharacterT
     */
    public static function unPack($union_type, $accessor)
    {
        switch ($union_type) {
            case Character::MuLan:
                $obj = $accessor(new \Attacker());
                return new CharacterT($union_type, $obj->unPack());
            case Character::Rapunzel:
                $obj = $accessor(new \Rapunzel());
                return new CharacterT($union_type, $obj->unPack());
            case Character::Belle:
                $obj = $accessor(new \BookReader());
                return new CharacterT($union_type, $obj->unPack());
            case Character::BookFan:
                $obj = $accessor(new \BookReader());
                return new CharacterT($union_type, $obj->unPack());
            case Character::Other:
                $obj = $accessor();
                return new CharacterT($union_type, $obj->unPack());
            case Character::Unused:
                $obj = $accessor();
                return new CharacterT($union_type, $obj->unPack());
            default:
                return null;
        }
    }
}
