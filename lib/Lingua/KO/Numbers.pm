package Lingua::KO::Numbers;

use strict;
use 5.8.1;
use warnings;
use utf8;
our $VERSION = '0.01';
use parent qw(Exporter);

our @EXPORT = qw(
    ko2num num2ko ko_to_number number_to_ko
);

use overload 
    q("") => \&stringify,
    q(0+) => \&numify,
    fallback => 1,
;

sub new {
    my($class, $str, $opt) = @_;
    my $val = $str ? ko2num($str, $opt) : '';
    return bless +{
        val => $val,
        opt => $opt || +{ style => 'hangul' },
    }, $class;
}

sub parse {
    my($self, $str, $opt) = @_;
    $opt ||= $self->{opt};
    $self->{val} = ko2num($str, $opt);
    $self->{opt} = $opt;
    return $self;
}

sub opt {
    my $self = shift;
    $self->{opt} = +{
        %{$self->{opt}},
        @_,
    };
    return $self;
}

sub numify { $_[0]->{val} }
*as_number = \&numify;
sub get_string { num2ko($_[0]->{val}, $_[0]->{opt}) }
*stringify = *as_string = \&get_string;

our $Zero = +{
    hangul => '영', # 공?
    romaja => 'Yeong',
};

our $Point = +{
    hangul => '점',
    romaja => 'Jeom',
};

our $Sign = +{
    hengle => +{'+' => q(), '-' => '−'},
    romaja => +{'+' => '+', '-' => '-'},
};

our $Zero2Nine = +{
    hangul => [qw(영 일 이 삼 사 오 육 칠 팔 구)],
    romaja => [qw(Yeong Il I Sam Sa O Yug Chil Pal Gu)],
};

our $Ten2Thou = +{
    hangul => [q(), qw(십 백 천)],
    romaja => [q(), qw(Sib Baeg Cheon)],
};

our $Hugenums = +{
    hangul => [qw(극 항하사 아승기 나유타 불가사의 무량대수)],
    romaja => [qw(Geug Hanghasa Aseung-gi Nayuta Bulgasaui Mulyangdaesu)],
};

our $Suffices = {
    hangul => [q(), qw(만 억 조 경 해 자 양 구 간 정 재),
              @{$Hugenums->{hangul}}],
    romaja => [q(), qw(Man Eog Jo Gyeong Hae Ja Yang Gu Gan Jeong Jae),
              @{$Hugenums->{romaja}}],
};

sub num2ko {
    no warnings 'uninitialized';
    my($num, $opt) = @_;
    my $style = $opt->{style} || 'hangul';
    my $zero = $opt->{zero} ? $opt->{zero} : $Zero->{$style};
    return $zero unless $num;
    my($sig, $int, $fract, $exp)
        = ($num =~ /([+-])?(\d+)(?:\.(\d+))?(?:[eE]([+-]?\d+))?/io);
    my $scientific = sub {
        my $first = substr($int, 0, 1, '');
        $exp += length($int);
        return num2ko("$sig$first.$int$fract" . "e$exp", $opt);
    };
    my $manman = '';
    if (length($int) > 48 and $opt->{manman}) {
        if (length($int) > 96) {
            return $scientific->();
        }
        $int =~ s/(.*)(.{48})\z/$2/o;
        my $huge = $1;
        my @shins;
        push @shins, $1 while $huge =~ s/(\d{8})$//g;
        push @shins, $huge if $huge;
        my $suffix = 0;
        for my $shin (@shins) {
            if ($shin eq '00000000') {
                $suffix++;
                next;
            }
            $manman =
                num2ko($shin, $opt) . $Hugenums->{$style}[$suffix++] . $manman;
        }
    }
    else {
        if (length($int) > 72) {
            return $scientific->();
        }
    }

    my $sign = $opt->{sign} ? $opt->{sign} : $Sign->{$style};
    my $zero2nine =
        $opt->{zero2nine} ? $opt->{zero2nine} : $Zero2Nine->{$style};
    my $ten2thou = $opt->{ten2thou} ? $opt->{ten2thou} : $Ten2Thou->{$style};
    my $suffices = $opt->{suffices} ? $opt->{suffices} : $Suffices->{$style};

    my($jeongsu, $jeogeum, $jisu) = ();
    my @shins;
    push @shins, $1 while $int =~ s/(\d{4})$//g;
    push @shins, $int if $int;
    my $suffix = 0;
    for my $shin (@shins) {
        if ($shin eq '0000') {
            $suffix++;
            next;
        }
        my $sens  = '';
        my $jalis = 0;
        for my $digit (reverse split //, $shin) {
            if ($opt->{fixed4} or $opt->{with_arabic}) {
                $sens =
                    ($opt->{with_arabic} ? $digit : $zero2nine->[$digit]) .
                        $sens;
            }
            else {
                my $susja =
                    ($digit == 1 && !$opt->{p_one} && $jalis > 0) ?
                        '' : $zero2nine->[$digit];
                $sens = $susja . $ten2thou->[$jalis] . $sens if $digit != 0;
            }
            $jalis++;
        }
        $suffix++;
        if ($suffix == 2 && $sens eq $zero2nine->[1]) {
            $jeongsu = $suffices->[$suffix - 1] . $jeongsu;
        }
        else {
            $jeongsu = $sens . $suffices->[$suffix - 1] . $jeongsu;
        }
    }
    my $result = $sign->{$sig} . $manman . $jeongsu;
    $result ||= $zero;
    if ($fract) {
        while ($fract =~ /(\d)/g) {
            $jeogeum .= $zero2nine->[$1];
        }
        my $point = $opt->{point} ? $opt->{point} : $Point->{$style};
        $result .= $point . $jeogeum;
    }
    if ($exp) {
        $result .=
            $opt->{romaja} ? 'Gobhagi Sib Ui ' . num2ko($exp, $opt) . ' Seung' :
                '곱하기 십의 ' . num2ko($exp, $opt) . ' 승';
    }
    return $result;
}

*number_to_ko = \&num2ko;

our %RE_Points = (
   '．'   => '.',
   '점'   => '.',
);

our $RE_Points = join('|', keys %RE_Points);

our %RE_Zero2Nine = (
    '영' => 0, '공' => 0, Yeong  => 0, Gong => 0,
    '일' => 1,            Il   => 1,
    '이' => 2,            I    => 2,
    '삼' => 3,            Sam  => 3,
    '사' => 4,            Sa   => 4,
    '오' => 5,            O    => 5,
    '육' => 6,            Yug  => 6,
    '칠' => 7,            Chil => 7,
    '팔' => 8,            Pal  => 8,
    '구' => 9,            Gu   => 9,
);

our $RE_Zero2Nine = join('|', keys %RE_Zero2Nine);

our %RE_Ten2Thou = ( 
    '십' => 1, Sib   => 1,
    '백' => 2, Baeg  => 2,
    '천' => 3, Cheon => 3,
);

our $RE_Ten2Thou = join('|', keys %RE_Ten2Thou);

our %RE_Suffices = (
    '만'       => 4,  Man          => 4,
    '억'       => 8,  Eog          => 8,
    '조'       => 12, Jo           => 12,
    '경'       => 16, Gyeong       => 16,
    '해'       => 20, Hae          => 20,
    '자'       => 24, Ja           => 24,
    '양'       => 28, Yang         => 28,
    '구'       => 32, Gu           => 32,
    '간'       => 36, Gan          => 36,
    '정'       => 40, Jeong        => 40,
    '재'       => 44, Jae          => 44,
    '극'       => 48, Geug         => 48,
    '항하사'   => 52, Hanghasa     => 52,
    '아승기'   => 56, 'Aseung-gi'  => 56,
    '나유타'   => 60, Nayuta       => 60,
    '불가사의' => 64, Bulgasaui    => 64,
    '무량대수' => 68, Mulyangdaesu => 68,
);

our $RE_Suffices = join('|', keys %RE_Suffices);

our %RE_Hugenums = (
    '극'       => 48, Geug         => 48,
    '항하사'   => 52, Hanghasa     => 52,
    '아승기'   => 56, 'Aseung-gi'  => 56,
    '나유타'   => 60, Nayuta       => 60,
    '불가사의' => 64, Bulgasaui    => 64,
    '무량대수' => 68, Mulyangdaesu => 68,
);

our $RE_Hugenums = join('|', keys %RE_Hugenums);

our %RE_Fraction = (
    '분'   => 2,  Bun         => 2,
    '리'   => 3,  Li          => 3,
    '모'   => 4,  Mo          => 4, '호' => 4, Ho => 4,
    '사'   => 5,  Sa          => 5,
    '홀'   => 6,  Hol         => 6,
    '미'   => 7,  Mi          => 7,
    '섬'   => 8,  Seom        => 8,
    '사'   => 9,  Sa          => 9,
    '진'   => 10, Jin         => 10,
    '애'   => 11, Ae          => 11,
    '묘'   => 12, Myo         => 12,
    '막'   => 13, Mag         => 13,
    '모호' => 14, Moho        => 14,
    '준순' => 15, Junsun      => 15,
    '수유' => 16, Suyu        => 16,
    '순식' => 17, Sunsig      => 17,
    '탄지' => 18, Tanji       => 18,
    '찰나' => 19, Chalna      => 19,
    '육덕' => 20, Yugdeog     => 20,
    '허공' => 21, Heogong     => 21,
    '청정' => 22, Cheongjeong => 22,
);

our $RE_Fraction = join('|', keys %RE_Fraction);

our %RE_Op = (
    '곱하기' => '*', Gobhagi => '*',
    '나누기' => '/', Nanugi  => '/',
    '더하기' => '+', Deohagi => '+', '플러스'   => '+', Peulleoseu => '+',
    Plus     => '+',
    '빼기'   => '-', Ppaegi  => '-', '마이너스' => '-', Maineoseu =>  '-',
    Minus    => '-',
);

our $RE_Op = join('|' => map { quotemeta($_) } keys %RE_Op);

our $RE_Numerals =
    qr{(?:\d
    |$RE_Zero2Nine|$RE_Ten2Thou|$RE_Suffices|$RE_Fraction|$RE_Points)+}ixo;

sub ko2num{
    no warnings 'uninitialized';
    my($ko, $opt) = @_;
    {
        no warnings 'numeric';
        my $num = $ko + 0;
        return $num if $num;
    }
    $ko or return;
    $ko =~ s/[\s\x{3000}]//g;
    $ko =~ tr[０-９][0-9];
    $ko =~ s{ (?:의|Ui|E)\s*($RE_Numerals)\s*(?:승|Seung) }
            { "**" . $1 }iegx;
    $ko =~ s{ ($RE_Numerals)  }{ _ko2num($1, $opt) }iegx;               
    $ko =~ s{ ($RE_Op) }{ $RE_Op{ucfirst $1} }igx;
    $ko =~ tr[（）＋−×÷][\(\)\+\-\*\/];
    $ko =~ tr/[G-Z]//d; 
    my $result = eval qq{ use bignum; $ko};
    $@ and $opt->{debug} and warn "$ko => $@";
    $opt->{debug} and warn qq{ko2num("$ko") == $result};
    return qq($result);
}
*ko_to_number = \&ko2num;

sub _ko2num{
    no warnings 'uninitialized';
    my($ko, $opt) = @_;
    $ko or return;
    my $manman = '';
    if ($opt->{manman}){ # wierd hack
        $ko =~ s{ \G(.*?)($RE_Hugenums) }
                { my ($p, $q) = ($1, $2);
                  $p ||= 1;
                  $manman .= _ko2num($p, $opt) . "e" . $RE_Hugenums{$q} . '+';
                  q();
                }iegx;
    }
    $ko =~ s{ ($RE_Zero2Nine) }{$RE_Zero2Nine{ucfirst $1}}igx;
    $ko =~ s{ (\d*)($RE_Ten2Thou)  }
                { my $n = $1 || 1;
                  $n.'e'.$RE_Ten2Thou{ucfirst $2}.'+' }iegx;
    $ko =~ s{ ([\d\+\-e]+)($RE_Fraction)  }
                { qq{($1)} . '*1e-' . $RE_Fraction{ucfirst $2} . '+'}iegx;
    $ko =~ s{ \G(.*?)\+?($RE_Suffices) }
            { my $p = $1 || 1;
              "($p)*1e" . $RE_Suffices{ucfirst $2} . '+' 
             }iegx;
    $ko =~ s{ ($RE_Points) }{ '.' }iegx;
    $ko = $manman . $ko;
    $ko =~ s{ \+\s*(\)|\z) }{$1}gx;
    # warn $ko;
    my $result = eval qq{ use bignum; $ko };
    $@ and $opt->{debug} and warn "$ko =>\n $@";
    $opt->{debug} and warn qq{_ko2num("$ko") == $result};
    return qq($result);
}

sub to_string{
    my ($str,$opt) = @_;
    $opt ||= +{};
    $opt->{style} = "hangul";
    my $ko = __PACKAGE__->new($str, $opt);
    my @words = map { lc $_ } ($ko->get_string =~ /([A-Z][a-z]*)/g);
    return @words;
}

1;
__END__

=head1 NAME

Lingua::KO::Numbers - Converts numeric values into their Korean string equivalents and vice versa

=head1 SYNOPSIS

  use Lingua::KO::Numbers;

  # OO Style
  my $ko = Lingua::KO::Numbers->new(1234567890, {style=>'romaja'});
  # SibIEogSamCheonSaBaegOSibYugManChilCheonPalBaegGuSib
  # $ko->get_string is imlictly called
  print "$ko\n";
  print $ko+0, "\n";
  # 1234567890
  # $ko->number is implicitly called.
  # 1234567890

  # Functional Style
  my $str = num2ko(1234567890, {style=>'romaja'});
  print "$str\n";
  # SibIEogSamCheonSaBaegOSibYugManChilCheonPalBaegGuSib
  print ko2num($str), "\n";
  # 1234567890

=head1 DESCRIPTION

This module converts Korean text (Hangul) in UTF-8 (or romaja in ascii) to number, AND vice versa.  Though this pod is in English and all examples are in romaja to make L<http://search.cpan.org/> happy, this module does accept Hangul in UTF-8. Try the code below to see it.

   perl -MLingua::KO::Numbers \
       -e '$y="\x{c774}\x{cc9c}\x{c2ed}\x{c0ac}"; printf "(C) %d Koichi Taniguchi\n", ko2num($y)'

Lingua::KO::Numbers is

=head1 AUTHOR

Koichi Taniguchi E<lt>taniguchi@cpan.orgE<gt>

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 SEE ALSO

L<Lingua::JA::Numbers>

=cut
